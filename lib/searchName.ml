(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Logs = Geneweb_logs.Logs
module Collection = Geneweb_db.Collection

module PerSet = Set.Make (struct
  type t = Driver.iper

  let compare = compare
end)

type match_source = DirectMatch | FirstNameAlias of string

type search_result_with_info = {
  iper : Driver.Iper.t;
  match_source : match_source;
}

type search_type =
  | Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey

(* FIXME this set of options needs deeper review
   for semantic and implementation *)
type opts = {
  order : bool; (* first_names should be in same order as typed *)
  all : bool; (* all first_names typed should be present *)
  case : bool; (* maintain case and accents when comparing *)
  exact : bool; (* fuzzy match (for the time being starts_with *)
  all_in : bool; (* all first_names should match one of the typed first_names *)
}

type search_results = {
  exact : Driver.Iper.t list; (* résultats exacts *)
  partial : Driver.Iper.t list; (* résultats partiels *)
  spouse : Driver.Iper.t list; (* résultats avec nom d'époux *)
}

(* Generate all apostrophe variants of a string *)
let generate_apostrophe_variants s =
  let apostrophes =
    [
      "'";
      (* U+0027 APOSTROPHE *)
      "\xE2\x80\x99";
      (* U+2019 RIGHT SINGLE QUOTATION MARK *)
      "\xCA\xBC";
      (* U+02BC MODIFIER LETTER APOSTROPHE *)
      "\xCA\xBB";
      (* U+02BB MODIFIER LETTER TURNED COMMA *)
    ]
  in
  let rec find_apostrophe_pos s i =
    if i >= String.length s then None
    else
      match s.[i] with
      | '\'' -> Some (i, 1)
      | '\xE2'
        when i + 2 < String.length s && s.[i + 1] = '\x80' && s.[i + 2] = '\x99'
        ->
          Some (i, 3)
      | '\xCA'
        when i + 1 < String.length s
             && (s.[i + 1] = '\xBC' || s.[i + 1] = '\xBB') ->
          Some (i, 2)
      | _ -> find_apostrophe_pos s (i + 1)
  in
  match find_apostrophe_pos s 0 with
  | None -> [ s ]
  | Some (pos, len) ->
      let variants =
        List.map
          (fun apo ->
            String.sub s 0 pos ^ apo
            ^ String.sub s (pos + len) (String.length s - pos - len))
          apostrophes
      in
      List.sort_uniq String.compare variants

let has_apostrophe s =
  String.contains s '\''
  || (try
        String.index s '\xE2' |> fun i ->
        i + 2 < String.length s && s.[i + 1] = '\x80' && s.[i + 2] = '\x99'
      with Not_found -> false)
  ||
  try
    String.index s '\xCA' |> fun i ->
    i + 1 < String.length s && (s.[i + 1] = '\xBC' || s.[i + 1] = '\xBB')
  with Not_found -> false

let empty_sn_or_fn base p =
  Driver.Istr.is_empty (Driver.get_surname p)
  || Driver.Istr.is_quest (Driver.get_surname p)
  || Driver.Istr.is_empty (Driver.get_first_name p)
  || Driver.Istr.is_quest (Driver.get_first_name p)
  || Name.lower (Driver.sou base (Driver.get_surname p)) = ""
  || Name.lower (Driver.sou base (Driver.get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if
    List.exists
      (fun n -> Name.strip n = k)
      (Driver.person_misc_names base p (nobtit conf base))
  then true
  else false

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (Driver.p_first_name base p) in
  let sn = Name.strip_lower (Driver.p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then true else false

(* Select individuals matching key k and stock matching alias in AliasCache *)
let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      let iper = Driver.get_iper p in
      if person_is_approx_key base p k then (
        Some.AliasCache.add_direct iper;
        p :: pl)
      else
        let k_stripped = Name.strip_lower k in
        let aliases = Driver.get_aliases p in
        let matched_alias =
          List.find_opt
            (fun alias_istr ->
              let alias_str = Driver.sou base alias_istr in
              Name.strip_lower alias_str = k_stripped)
            aliases
        in
        match matched_alias with
        | Some alias_istr ->
            let alias_str = Driver.sou base alias_istr in
            Some.AliasCache.add_alias iper alias_str;
            p :: pl
        | None ->
            if person_is_misc_name conf base p k then (
              Some.AliasCache.add_direct iper;
              p :: pl)
            else pl)
    pl []

let split_normalize case s =
  let s = Name.abbrev s in
  let s = if case then s else Name.lower s in
  cut_words s

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | None, _ | _, None -> None
  | Some p, Some n when n <> Sosa.zero -> (
      match
        Util.branch_of_sosa conf base n (pget conf base @@ Driver.get_iper p)
      with
      | Some (p :: _) -> Some p
      | _ -> None)
  | _ -> None

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index n1 ' ' with
  | exception Not_found -> []
  | i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let p_of_sn_l, _ =
        Some.persons_of_fsname conf base Driver.base_strings_of_surname
          (Driver.spi_find (Driver.persons_of_surname base))
          Driver.get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              match Util.pget_opt conf base ip with
              | None -> pl
              | Some p ->
                  let fn1_l =
                    split_normalize true
                      (Driver.sou base (Driver.get_first_name p))
                  in
                  let fn2_l =
                    split_normalize true
                      (Driver.sou base (Driver.get_public_name p))
                  in
                  if List.mem fn fn1_l || List.mem fn fn2_l then p :: pl else pl)
            pl ipl)
        [] p_of_sn_l

let search_key_aux aux conf base an =
  let acc = Gutil.person_not_a_key_find_all base an in
  let an, acc =
    if acc = [] then
      match Util.name_with_roman_number an with
      | Some an1 ->
          let acc = Gutil.person_ht_find_all base an1 in
          if acc = [] then (an, []) else (an1, acc)
      | None -> (an, acc)
    else (an, acc)
  in
  let acc = Mutil.filter_map (fun i -> Util.pget_opt conf base i) acc in
  let acc = aux conf base acc an in
  Gutil.sort_uniq_person_list base acc

let search_approx_key = search_key_aux select_approx_key

let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
  | None -> None
  | Some ip -> Util.pget_opt conf base ip

(* Gestionnaire centralisé des duplicatas *)
module DuplicateManager = struct
  let create () = Hashtbl.create 100

  let add_if_new ht ip =
    if Hashtbl.mem ht ip then false
    else (
      Hashtbl.add ht ip ();
      true)

  let filter_new ht ips = List.filter (add_if_new ht) ips
end

(* Cache pour éviter les appels répétés à Driver.sou *)
module StringCache = struct
  let cache = Hashtbl.create 2000
  let max_size = 5000
  let hits = ref 0
  let misses = ref 0

  let get_cached base istr =
    try
      incr hits;
      Hashtbl.find cache istr
    with Not_found ->
      incr misses;
      let s = Driver.sou base istr in
      if Hashtbl.length cache < max_size then Hashtbl.add cache istr s;
      s

  let clear_if_full () =
    if Hashtbl.length cache > max_size then (
      Logs.debug (fun k ->
          k "StringCache clearing cache: %d hits, %d misses" !hits !misses);
      Hashtbl.clear cache;
      hits := 0;
      misses := 0)

  let maintenance () = clear_if_full ()
end

(* Convertit une liste de persons en liste d’ipers *)
let persons_to_ipers pl = List.map (fun p -> Driver.get_iper p) pl

(* Transformer les options en listes *)
(* Recherche par Sosa optimisée *)
let search_sosa_opt conf base query =
  match search_by_sosa conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

(* Recherche par clé optimisée *)
let search_key_opt conf base query =
  match search_by_key conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

let match_fn_lists fn_l fn1_l opts =
  let normalize s = if opts.case then s else Name.lower s in
  let word_matches query_word person_word =
    let q = normalize query_word in
    let p = normalize person_word in
    if opts.exact then q = p else Mutil.contains p q
  in
  let check_order =
    if not opts.order then true
    else
      let rec check_in_order query_list person_list =
        match query_list with
        | [] -> true
        | q :: qs ->
            let rec find_and_continue pl =
              match pl with
              | [] -> false
              | p :: rest ->
                  if word_matches q p then check_in_order qs rest
                  else find_and_continue rest
            in
            find_and_continue person_list
      in
      check_in_order fn_l fn1_l
  in
  let basic_match =
    if opts.all then
      List.for_all (fun q -> List.exists (word_matches q) fn1_l) fn_l
    else List.exists (fun q -> List.exists (word_matches q) fn1_l) fn_l
  in
  basic_match && check_order

let rec list_take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: list_take (n - 1) xs

let rec list_drop n = function
  | xs when n <= 0 -> xs
  | [] -> []
  | _ :: xs -> list_drop (n - 1) xs

let search_for_multiple_fn conf base fn pl opts batch_size =
  let fn_l = cut_words fn in
  let rec process_batch acc remaining =
    match remaining with
    | [] -> acc
    | _ ->
        let batch, rest =
          if List.length remaining <= batch_size then (remaining, [])
          else (list_take batch_size remaining, list_drop batch_size remaining)
        in
        let batch_results =
          List.fold_left
            (fun acc_batch p ->
              if search_reject_p conf base p then acc_batch
              else
                let fn1_istr = Driver.get_first_name p in
                let fn1 = StringCache.get_cached base fn1_istr in
                let fn1_l = split_normalize opts.case fn1 in

                let fn2_istr = Driver.get_public_name p in
                let fn2 = StringCache.get_cached base fn2_istr in
                let fn2_l = split_normalize opts.case fn2 in

                if
                  match_fn_lists fn_l fn1_l opts
                  || match_fn_lists fn_l fn2_l opts
                then p :: acc_batch
                else acc_batch)
            [] batch
        in
        process_batch (batch_results @ acc) rest
  in
  process_batch [] pl

let rec search_surname conf base x =
  Logs.debug (fun k -> k "Search_surname: %s" x);
  let has_apostrophe = has_apostrophe x in
  match has_apostrophe with
  | false -> search_phonetic conf base x
  | true ->
      Logs.debug (fun k ->
          k "  Apostrophe detected, trying exact variants first");
      let variants = generate_apostrophe_variants x in
      let exact_results = search_exact conf base variants in

      if exact_results <> [] then (
        Logs.debug (fun k ->
            k "  exact: %d results" (List.length exact_results));
        exact_results)
      else (
        Logs.debug (fun k -> k "  exact: 0, trying phonetic search");
        let fallback_query = List.hd variants in
        search_phonetic conf base fallback_query)

and search_exact conf base variants =
  let exact_iperl = ref PerSet.empty in
  let found_matches = ref [] in
  List.iter
    (fun variant ->
      try
        let list, _name_inj =
          Some.persons_of_fsname conf base Driver.base_strings_of_surname
            (Driver.spi_find (Driver.persons_of_surname base))
            Driver.get_surname variant
        in
        List.iter
          (fun (str, _, iperl) ->
            if Name.lower str = Name.lower variant then (
              found_matches := (str, List.length iperl) :: !found_matches;
              List.iter
                (fun ip -> exact_iperl := PerSet.add ip !exact_iperl)
                iperl))
          list
      with _ -> ())
    variants;
  if !found_matches <> [] then
    Logs.debug (fun k ->
        k "  matches: %s"
          (String.concat ", "
             (List.map
                (fun (str, count) -> Printf.sprintf "%s(%d)" str count)
                !found_matches)));
  PerSet.elements !exact_iperl

and search_phonetic conf base query =
  try
    let list, _name_inj =
      Some.persons_of_fsname conf base Driver.base_strings_of_surname
        (Driver.spi_find (Driver.persons_of_surname base))
        Driver.get_surname query
    in
    let ddr_iperl = ref PerSet.empty in
    List.iter
      (fun (_, _, iperl) ->
        List.iter (fun ip -> ddr_iperl := PerSet.add ip !ddr_iperl) iperl)
      list;
    let results = PerSet.elements !ddr_iperl in
    Logs.debug (fun k -> k "  phonetic: %d results" (List.length results));
    results
  with _ ->
    Logs.debug (fun k -> k "  phonetic: failed");
    []

(* Recherche directe de prénom via l'index *)
let search_firstname_direct conf base query =
  let list, _name_inj =
    if query = "" then ([], fun x -> x)
    else
      Some.persons_of_fsname conf base Driver.base_strings_of_first_name
        (Driver.spi_find (Driver.persons_of_first_name base))
        Driver.get_first_name query
  in
  let result = ref [] in
  let seen = Hashtbl.create 1000 in
  List.iter
    (fun (_, _, iperl) ->
      List.iter
        (fun ip ->
          if not (Hashtbl.mem seen ip) then (
            Hashtbl.add seen ip ();
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if
              fn <> ""
              && (not (Driver.Istr.is_empty (Driver.get_first_name p)))
              && (not (Driver.Istr.is_empty (Driver.get_surname p)))
              && not
                   (Util.is_hide_names conf p
                   && not (Util.authorized_age conf base p))
            then result := ip :: !result))
        iperl)
    list;
  List.rev !result

let search_firstname_aliases conf base query =
  let query_lower = Name.lower query in
  let all_misc_matches = Gutil.person_not_a_key_find_all base query in
  List.fold_left
    (fun acc ip ->
      let p = Driver.poi base ip in
      if
        empty_sn_or_fn base p
        || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))
      then acc
      else
        let aliases = Driver.get_first_names_aliases p in
        match
          List.find_opt
            (fun alias_istr ->
              let alias_str = Driver.sou base alias_istr in
              Name.lower alias_str = query_lower)
            aliases
        with
        | Some alias_istr ->
            let alias_str = Driver.sou base alias_istr in
            (ip, alias_str) :: acc
        | None -> acc)
    [] all_misc_matches

let search_firstname_with_aliases conf base query =
  let direct_results = search_firstname_direct conf base query in
  let alias_results = search_firstname_aliases conf base query in

  List.iter
    (fun (iper, alias) -> Some.AliasCache.add_alias iper alias)
    alias_results;
  List.iter (fun iper -> Some.AliasCache.add_direct iper) direct_results;

  let direct_with_info =
    List.map (fun iper -> { iper; match_source = DirectMatch }) direct_results
  in
  let alias_with_info =
    List.map
      (fun (iper, alias) -> { iper; match_source = FirstNameAlias alias })
      alias_results
  in

  direct_with_info @ alias_with_info

(* Recherche de prénom optimisée avec cache et accès direct à l'index *)
let search_firstname_with_cache conf base query opts =
  let query_words = cut_words query in
  Logs.debug (fun k ->
      k "    Searching for words: [%s]" (String.concat "; " query_words));

  let all_results =
    if opts.all && List.length query_words > 1 then (
      (* Si all=true avec plusieurs mots *)
      let search_queries =
        if opts.order then
          (* Si order=on, générer toutes les permutations *)
          let rec permutations = function
            | [] -> [ [] ]
            | x :: xs ->
                let perms = permutations xs in
                List.flatten
                  (List.map
                     (fun p ->
                       let rec insert_everywhere e = function
                         | [] -> [ [ e ] ]
                         | h :: t ->
                             (e :: h :: t)
                             :: List.map
                                  (fun l -> h :: l)
                                  (insert_everywhere e t)
                       in
                       insert_everywhere x p)
                     perms)
          in
          let perms = permutations query_words in
          List.map (fun words -> String.concat " " words) perms
        else [ query ]
      in

      Logs.debug (fun k ->
          k "    Searching %d permutation(s): %s"
            (List.length search_queries)
            (String.concat ", " search_queries));

      (* Chercher chaque permutation dans l'index *)
      let all_found = ref [] in
      let seen = Hashtbl.create 1000 in

      List.iter
        (fun search_query ->
          let res = search_firstname_with_aliases conf base search_query in
          Logs.debug (fun k ->
              k "      Query '%s': %d results" search_query (List.length res));
          List.iter
            (fun r ->
              if not (Hashtbl.mem seen r.iper) then (
                Hashtbl.add seen r.iper ();
                all_found := r :: !all_found))
            res)
        search_queries;

      List.rev !all_found)
    else if opts.all then (
      (* Un seul mot avec all=true *)
      let res = search_firstname_with_aliases conf base query in
      Logs.debug (fun k ->
          k "    Single word search '%s': %d results" query (List.length res));
      res)
    else
      (* Si all=false, on cherche chaque mot séparément *)
      let results_per_word =
        List.map
          (fun word ->
            Logs.debug (fun k -> k "    Searching index for word: %s" word);
            let res = search_firstname_with_aliases conf base word in
            Logs.debug (fun k ->
                k "      Found %d results for '%s'" (List.length res) word);
            res)
          query_words
      in
      let all_ips = List.flatten results_per_word in
      let seen = Hashtbl.create 1000 in
      List.filter
        (fun r ->
          if Hashtbl.mem seen r.iper then false
          else (
            Hashtbl.add seen r.iper ();
            true))
        all_ips
  in

  Logs.debug (fun k -> k "    Total candidates: %d" (List.length all_results));

  (* Collecte des variantes de prénoms SEULEMENT pour DirectMatch *)
  let firstname_variants = ref Mutil.StrSet.empty in

  if not opts.all then (
    (* Si all=false, tous les candidats sont exacts *)
    List.iter
      (fun result ->
        match result.match_source with
        | DirectMatch ->
            let p = Driver.poi base result.iper in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then
              firstname_variants := Mutil.StrSet.add fn !firstname_variants
        | FirstNameAlias _ -> ())
      all_results;
    (List.map (fun r -> r.iper) all_results, [], !firstname_variants))
  else
    (* Si all=true, les résultats sont déjà corrects *)
    let exact = ref [] in
    let exact_count = ref 0 in
    let direct_match_count = ref 0 in
    let alias_match_count = ref 0 in

    List.iter
      (fun result ->
        let ip = result.iper in
        exact := ip :: !exact;
        incr exact_count;
        match result.match_source with
        | DirectMatch ->
            incr direct_match_count;
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then
              firstname_variants := Mutil.StrSet.add fn !firstname_variants
        | FirstNameAlias _ -> incr alias_match_count)
      all_results;

    Logs.debug (fun k ->
        k
          "    Final: %d exact (%d direct, %d alias), (%d unique firstnames \
           collected as variants)"
          !exact_count !direct_match_count !alias_match_count
          (Mutil.StrSet.cardinal !firstname_variants));
    (List.rev !exact, [], !firstname_variants)

let group_by_surname base ipers =
  let groups = Hashtbl.create 10 in
  List.iter
    (fun ip ->
      let p = Driver.poi base ip in
      let sn = Driver.sou base (Driver.get_surname p) in
      let current = try Hashtbl.find groups sn with Not_found -> [] in
      Hashtbl.replace groups sn (p :: current))
    ipers;
  Hashtbl.fold (fun sn persons acc -> (sn, List.rev persons) :: acc) groups []

let search_fullname conf base fn sn =
  let conf_sn =
    {
      conf with
      env =
        ("surname", Adef.encoded sn)
        :: ("exact_surname", Adef.encoded "on")
        :: conf.env;
    }
  in
  let persons, _ = AdvSearchOk.advanced_search conf_sn base max_int in
  Logs.debug (fun k ->
      k "  Method search_fullname: %d results" (List.length persons));

  match persons with
  | [] -> { exact = []; partial = []; spouse = [] }
  | [ p ] -> { exact = [ Driver.get_iper p ]; partial = []; spouse = [] }
  | pl ->
      let opts =
        {
          order = false;
          all = true;
          case = false;
          exact = false;
          all_in = true;
        }
      in
      let exact = search_for_multiple_fn conf base fn pl opts 1000 in

      let opts_partial = { opts with all_in = false } in
      let partial = search_for_multiple_fn conf base fn pl opts_partial 1000 in

      let spouse =
        if List.assoc_opt "public_name_as_fn" conf.base_env <> Some "no" then
          let sn_bearers = search_surname conf base sn in
          let spouses =
            List.fold_left
              (fun acc ip ->
                let p = Driver.poi base ip in
                Array.fold_left
                  (fun acc ifam ->
                    let f = Driver.foi base ifam in
                    let spouse_ip =
                      if ip = Driver.get_father f then Driver.get_mother f
                      else Driver.get_father f
                    in
                    Driver.poi base spouse_ip :: acc)
                  acc (Driver.get_family p))
              [] sn_bearers
          in
          search_for_multiple_fn conf base fn spouses opts_partial 1000
        else []
      in
      {
        exact = persons_to_ipers exact;
        partial = persons_to_ipers partial;
        spouse = persons_to_ipers spouse;
      }

(* Recherche par clé partielle *)
let search_partial_key conf base query =
  let pl = search_by_name conf base query in

  Logs.debug (fun k ->
      k "  Method search_partial_key: %d results" (List.length pl));
  match pl with
  | [] ->
      let n1 = Name.abbrev (Name.lower query) in
      let fn, sn =
        match String.index_opt n1 ' ' with
        | Some i ->
            (String.sub n1 0 i, String.sub n1 (i + 1) (String.length n1 - i - 1))
        | _ -> ("", n1)
      in
      let conf = { conf with env = ("surname", Adef.encoded sn) :: conf.env } in
      let persons, _ = AdvSearchOk.advanced_search conf base max_int in
      if persons = [] then { exact = []; partial = []; spouse = [] }
      else
        let opts =
          {
            order = false;
            all = true;
            case = false;
            exact = false;
            all_in = true;
          }
        in
        let opts_exact = { opts with all_in = true } in
        let opts_partial = { opts with all_in = false } in
        let exact =
          search_for_multiple_fn conf base fn persons opts_exact 1000
        in
        let partial =
          search_for_multiple_fn conf base fn persons opts_partial 1000
        in
        {
          exact = persons_to_ipers exact;
          partial = persons_to_ipers partial;
          spouse = [];
        }
  | [ p ] -> { exact = [ Driver.get_iper p ]; partial = []; spouse = [] }
  | pl -> { exact = persons_to_ipers pl; partial = []; spouse = [] }

module ApostropheCache = struct
  let cache = Hashtbl.create 100

  let get_variants s =
    try Hashtbl.find cache s
    with Not_found ->
      let variants = generate_apostrophe_variants s in
      Hashtbl.add cache s variants;
      variants
end

let dispatch_search_methods conf base query search_order =
  let results = ref { exact = []; partial = []; spouse = [] } in
  let firstname_variants = ref Mutil.StrSet.empty in
  StringCache.maintenance ();
  List.iter
    (function
      | Sosa ->
          let ips = search_sosa_opt conf base query in
          Logs.debug (fun k -> k "  Method Sosa: %d results" (List.length ips));
          results := { !results with exact = !results.exact @ ips }
      | Key ->
          let ips = search_key_opt conf base query in
          Logs.debug (fun k -> k "  Method Key: %d results" (List.length ips));
          results := { !results with exact = !results.exact @ ips }
      | Surname ->
          let ips = search_surname conf base query in
          results := { !results with exact = !results.exact @ ips }
      | FirstName ->
          let opts =
            {
              order = p_getenv conf.env "p_order" = Some "on";
              all = p_getenv conf.env "p_all" <> Some "off";
              case = false;
              exact = p_getenv conf.env "p_exact" <> Some "off";
              all_in = false;
            }
          in
          Logs.debug (fun k ->
              k "  FirstName options: all=%b, exact=%b, order=%b" opts.all
                opts.exact opts.order);
          let exact, partial, fn_variants =
            search_firstname_with_cache conf base query opts
          in
          List.iter
            (fun fn ->
              firstname_variants := Mutil.StrSet.add fn !firstname_variants)
            (Mutil.StrSet.elements fn_variants);

          Logs.debug (fun k ->
              k "  Method FirstName: %d + %d results exact/partial"
                (List.length exact) (List.length partial));
          results :=
            {
              exact = !results.exact @ exact;
              partial = !results.partial @ partial;
              spouse = !results.spouse;
            }
      | FullName ->
          let fn, sn =
            match (p_getenv conf.env "p", p_getenv conf.env "n") with
            | Some fn, Some sn when fn <> "" && sn <> "" -> (fn, sn)
            | _ -> (
                match String.rindex_opt query ' ' with
                | Some i ->
                    ( String.sub query 0 i,
                      String.sub query (i + 1) (String.length query - i - 1) )
                | _ -> ("", query))
          in
          let r = search_fullname conf base fn sn in
          Logs.debug (fun k ->
              k "  Method FullName: %d + %d + %d results exact/partial/spouse"
                (List.length r.exact) (List.length r.partial)
                (List.length r.spouse));
          results :=
            {
              exact = !results.exact @ r.exact;
              partial = !results.partial @ r.partial;
              spouse = !results.spouse @ r.spouse;
            }
      | ApproxKey ->
          let pl = search_approx_key conf base query in
          let ips = List.map (fun p -> Driver.get_iper p) pl in
          Logs.debug (fun k ->
              k "  Method ApproxKey: %d results" (List.length ips));
          results := { !results with exact = !results.exact @ ips }
      | PartialKey ->
          let r = search_partial_key conf base query in
          Logs.debug (fun k ->
              k "  Method PartialKey: %d + %d + %d results"
                (List.length r.exact) (List.length r.partial)
                (List.length r.spouse));
          results :=
            {
              exact = !results.exact @ r.exact;
              partial = !results.partial @ r.partial;
              spouse = !results.spouse @ r.spouse;
            })
    search_order;
  let seen = DuplicateManager.create () in
  let exact = DuplicateManager.filter_new seen !results.exact in
  let partial = DuplicateManager.filter_new seen !results.partial in
  let spouse = DuplicateManager.filter_new seen !results.spouse in
  ({ exact; partial; spouse }, !firstname_variants)

let search conf base query search_order specify unknown =
  Some.AliasCache.clear ();
  let variant_cache = Hashtbl.create 10 in
  let needs_apostrophe_variants =
    List.exists
      (function
        | FirstName -> true (* Prénoms avec apostrophes *)
        | FullName -> true (* Noms complets avec apostrophes *)
        | Key -> true (* Clés peuvent avoir des apostrophes *)
        | Sosa -> true (* Sosa peuvent avoir des apostrophes *)
        | Surname -> false (* Surname gère ses propres apostrophes *)
        | ApproxKey | PartialKey -> false (* Pas besoin *))
      search_order
  in
  let variants =
    if needs_apostrophe_variants && has_apostrophe query then
      ApostropheCache.get_variants query
    else [ query ]
  in
  let has_variants = List.length variants > 1 in
  if has_variants then
    Logs.debug (fun k ->
        k "  %d apostrophe variants: %s" (List.length variants)
          (String.concat ", " variants));
  let all_results, collected_firstname_variants =
    List.fold_left
      (fun (acc_results, acc_variants) variant ->
        let r, fn_variants =
          try
            let cached = Hashtbl.find variant_cache variant in
            (cached, Mutil.StrSet.empty)
          with Not_found ->
            let res, variants =
              dispatch_search_methods conf base variant search_order
            in
            Hashtbl.add variant_cache variant res;
            (res, variants)
        in
        if r.exact <> [] || r.partial <> [] || r.spouse <> [] then
          Logs.debug (fun k ->
              k "%s: %d exact, %d partial, %d spouse" variant
                (List.length r.exact) (List.length r.partial)
                (List.length r.spouse));
        let merged_results =
          {
            exact = acc_results.exact @ r.exact;
            partial = acc_results.partial @ r.partial;
            spouse = acc_results.spouse @ r.spouse;
          }
        in
        let merged_variants = Mutil.StrSet.union acc_variants fn_variants in
        (merged_results, merged_variants))
      ({ exact = []; partial = []; spouse = [] }, Mutil.StrSet.empty)
      variants
  in
  let seen = DuplicateManager.create () in
  let final_results =
    {
      exact = DuplicateManager.filter_new seen all_results.exact;
      partial = DuplicateManager.filter_new seen all_results.partial;
      spouse = DuplicateManager.filter_new seen all_results.spouse;
    }
  in
  let all_ips =
    final_results.exact @ final_results.partial @ final_results.spouse
  in
  let total =
    List.length final_results.exact
    + List.length final_results.partial
    + List.length final_results.spouse
  in
  if total > 0 then
    Logs.debug (fun k ->
        k "Total: %d (%d+%d+%d)" total
          (List.length final_results.exact)
          (List.length final_results.partial)
          (List.length final_results.spouse));
  match all_ips with
  | [] -> SrcfileDisplay.print_welcome conf base
  | [ ip ] ->
      record_visited conf ip;
      Perso.print conf base (Driver.poi base ip)
  | _ -> (
      let pl1 = List.map (Driver.poi base) final_results.exact in
      let pl2 = List.map (Driver.poi base) final_results.partial in
      let pl3 = List.map (Driver.poi base) final_results.spouse in
      let surname_groups = lazy (group_by_surname base all_ips) in
      let get_surname_groups () = Lazy.force surname_groups in
      let fn =
        match p_getenv conf.env "p" with
        | Some "" | None -> None
        | Some s -> Some s
      in
      let sn =
        match p_getenv conf.env "n" with
        | Some "" | None -> None
        | Some s -> Some s
      in
      let pn =
        match p_getenv conf.env "pn" with
        | Some "" | None -> None
        | Some s -> Some s
      in

      match (fn, sn, pn) with
      (* CAS 1: PRÉNOM seul *)
      | Some _, None, None ->
          Logs.debug (fun k -> k "Case: First name only");
          SosaCache.build_sosa_ht conf base;
          let str =
            if Mutil.StrSet.is_empty collected_firstname_variants then
              List.fold_left
                (fun acc p ->
                  Mutil.StrSet.add
                    (Driver.sou base @@ Driver.get_first_name p)
                    acc)
                Mutil.StrSet.empty pl1
            else collected_firstname_variants
          in
          let tit2 =
            if pl2 <> [] then
              transl conf "other possibilities" |> Utf8.capitalize_fst
            else ""
          in
          let tit3 =
            if pl3 <> [] then
              transl conf "with spouse name" |> Utf8.capitalize_fst
            else ""
          in
          Some.first_name_print_list conf base query str
            [ ("", pl1); (tit2, pl2); (tit3, pl3) ]
      (* CAS 2: NOM DE FAMILLE seul *)
      | None, Some sn, None -> (
          let surname_groups = get_surname_groups () in
          match surname_groups with
          | [] -> assert false
          | [ (surname, _) ] ->
              Some.search_surname_print conf base unknown surname
          | multiple ->
              let surnames =
                List.fold_left (fun acc (surname, _) -> surname :: acc ) [] multiple
              in
              Some.print_several_possible_surnames sn conf base ([], surnames))
      (* CAS 3: Recherche avec PN (format prénom/nom/occ) *)
      | None, None, Some pn_value -> (
          Logs.debug (fun k -> k "Case: PN format");
          let i = try String.index pn_value '/' with Not_found -> -1 in
          if i = -1 then (
            Logs.debug (fun k -> k "Case: PN format, no /");
            match (pl1, pl2, pl3) with
            | [ p ], _, _ ->
                record_visited conf (Driver.get_iper p);
                Perso.print conf base p
            | _ -> specify conf base query pl1 pl2 pl3)
          else
            let j =
              try String.index_from pn_value (i + 1) '/' with Not_found -> -1
            in
            let oc =
              if j = -1 then ""
              else String.sub pn_value (j + 1) (String.length pn_value - j - 1)
            in
            let j = if j = -1 then 0 else String.length pn_value - j in
            let fn_part = String.sub pn_value 0 i |> String.trim in
            let sn_part =
              String.sub pn_value (i + 1) (String.length pn_value - i - 1 - j)
              |> String.trim
            in
            match (fn_part, sn_part, oc) with
            (* Prénom seul via pn *)
            | fn, "", "" when fn <> "" ->
                Logs.debug (fun k -> k "Prénom seul avec pn");
                let str =
                  List.fold_left
                    (fun acc p ->
                      Mutil.StrSet.add
                        (Driver.sou base @@ Driver.get_first_name p)
                        acc)
                    Mutil.StrSet.empty pl1
                in
                let tit2 =
                  if pl2 <> [] then
                    transl conf "other possibilities" |> Utf8.capitalize_fst
                  else ""
                in
                let tit3 =
                  if pl3 <> [] then
                    transl conf "with spouse name" |> Utf8.capitalize_fst
                  else ""
                in
                Some.first_name_print_list conf base query str
                  [ ("", pl1); (tit2, pl2); (tit3, pl3) ]
            (* Nom seul via pn *)
            | "", sn, "" when sn <> "" -> (
                Logs.debug (fun k -> k "Non seul avec pn");
                let surname_groups = get_surname_groups () in
                match surname_groups with
                | [] -> specify conf base query pl1 pl2 pl3
                | [ (surname, _) ] ->
                    Some.search_surname_print conf base unknown surname
                | multiple ->
                    if has_variants then
                      Some.print_multiple_display conf base query multiple
                    else specify conf base query pl1 pl2 pl3)
            (* Autres cas avec pn *)
            | _ ->
                Logs.debug (fun k -> k "Autres cas avec pn");
                specify conf base query pl1 pl2 pl3)
      (* CAS 4: Recherche PRÉNOM + NOM *)
      | Some _, Some _, None ->
          if conf.wizard then
            Logs.debug (fun k -> k "Case: First name + Surname\n");
          specify conf base query pl1 pl2 pl3
      (* CAS PAR DÉFAUT *)
      | _ ->
          if conf.wizard then Logs.debug (fun k -> k "Case: Default\n");
          specify conf base query pl1 pl2 pl3)

(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)

(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
    trouver la meilleure combinaison de résultat pour afficher la réponse la
    plus probable. [Args] :
    - conf : configuration de la base
    - base : base [Retour] : Néant [Rem] : Exporté en clair hors de ce module.
*)
let print conf base specify unknown =
  let real_input label =
    match p_getenv conf.env label with
    | Some s -> if s = "" then None else Some s
    | None -> None
  in
  match (real_input "pn", real_input "p", real_input "n") with
  | None, Some fn, Some sn ->
      let order = [ Key; FullName; ApproxKey; PartialKey; Surname ] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | None, Some fn, None ->
      let order = [ FirstName ] in
      search conf base fn order specify unknown
  | None, None, Some sn ->
      let order = [ Surname ] in
      search conf base sn order specify unknown
  | Some pn, None, None -> (
      let order = [ Sosa; Key; FullName; ApproxKey; PartialKey; Surname ] in
      let i = try String.index pn '/' with Not_found -> -1 in
      if i = -1 then search conf base pn order specify unknown
      else
        let j = try String.index_from pn (i + 1) '/' with Not_found -> -1 in
        let oc =
          if j = -1 then "" else String.sub pn (j + 1) (String.length pn - j - 1)
        in
        let j = if j = -1 then 0 else String.length pn - j in
        let fn = String.sub pn 0 i |> String.trim in
        let sn =
          String.sub pn (i + 1) (String.length pn - i - 1 - j) |> String.trim
        in
        match (fn, sn, oc) with
        | fn, "", "" when fn <> "" ->
            let order = [ FirstName ] in
            search conf base fn order specify unknown
        | "", sn, "" when sn <> "" ->
            let order = [ Surname; ApproxKey ] in
            search conf base sn order specify unknown
        | _ ->
            let order =
              [ Sosa; Key; FullName; ApproxKey; PartialKey; Surname ]
            in
            let env =
              List.map
                (fun (k, v) ->
                  match k with
                  | "p" -> (k, Adef.encoded fn)
                  | "n" -> (k, Adef.encoded sn)
                  | "oc" -> (k, Adef.encoded oc)
                  | _ -> (k, v))
                conf.env
            in
            let env =
              if List.mem_assoc "oc" env && oc <> "" then env
              else ("oc", Adef.encoded oc) :: env
            in
            let conf = { conf with env } in
            search conf base
              (Printf.sprintf "%s%s %s" (String.sub pn 0 i)
                 (if oc = "" then "" else "." ^ oc)
                 (String.sub pn (i + 1) (String.length pn - i - 1 - j)))
              order specify unknown)
  | _ -> SrcfileDisplay.print_welcome conf base
