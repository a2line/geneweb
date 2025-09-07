(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Logs = Geneweb_logs.Logs
module Collection = Geneweb_db.Collection

type match_source = DirectMatch | FirstNameAlias of string

type search_result_with_info = {
  iper : Driver.Iper.t;
  match_source : match_source;
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
  | None -> [ s ] (* No apostrophe, return original *)
  | Some (pos, len) ->
      (* Generate variant for each apostrophe type *)
      List.map
        (fun apo ->
          String.sub s 0 pos ^ apo
          ^ String.sub s (pos + len) (String.length s - pos - len))
        apostrophes

(* TODO use function from Util instead? *)
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

let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      if person_is_approx_key base p k then p :: pl
      else if person_is_misc_name conf base p k then p :: pl
      else pl)
    pl []

let split_normalize case s =
  let s = Name.abbrev s in
  let s = if case then s else Name.lower s in
  cut_words s

(* search functions *)
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

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
  | None -> None
  | Some ip -> Util.pget_opt conf base ip

(* main *)

type search_type =
  | Sosa
  | Key
  | SurnameExact
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey
  | DefaultSurname

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
  let module StrSet = Set.Make (String) in
  let normalize s = if opts.case then s else Name.lower s in
  let fn1_set =
    List.fold_left
      (fun set s -> StrSet.add (normalize s) set)
      StrSet.empty fn1_l
  in
  match (opts.all, opts.exact, opts.all_in) with
  | true, true, _ ->
      List.for_all (fun fn -> StrSet.mem (normalize fn) fn1_set) fn_l
  | true, false, _ ->
      List.for_all
        (fun fn ->
          StrSet.exists (fun fn1 -> Mutil.contains fn1 (normalize fn)) fn1_set)
        fn_l
  | false, true, true ->
      (* all_in=true: tous les fn1_l doivent être dans fn_l *)
      let fn_set =
        List.fold_left
          (fun set s -> StrSet.add (normalize s) set)
          StrSet.empty fn_l
      in
      List.for_all (fun fn1 -> StrSet.mem (normalize fn1) fn_set) fn1_l
  | false, true, false ->
      (* all_in=false: au moins un fn_l doit être dans fn1_l *)
      List.for_all (fun fn -> StrSet.mem (normalize fn) fn1_set) fn_l
  | false, false, _ ->
      List.exists
        (fun fn ->
          StrSet.exists (fun fn1 -> Mutil.contains fn1 (normalize fn)) fn1_set)
        fn_l

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
  let result = ref [] in
  let seen = Hashtbl.create 1000 in
  let query_lower = Name.lower query in

  Collection.iter
    (fun iper ->
      if not (Hashtbl.mem seen iper) then
        let p = Driver.poi base iper in
        let aliases = Driver.get_first_names_aliases p in
        List.iter
          (fun alias_istr ->
            let alias_str = Driver.sou base alias_istr in
            if Name.lower alias_str = query_lower then (
              Hashtbl.add seen iper ();
              if
                alias_str <> ""
                && (not (Driver.Istr.is_empty (Driver.get_first_name p)))
                && (not (Driver.Istr.is_empty (Driver.get_surname p)))
                && not
                     (Util.is_hide_names conf p
                     && not (Util.authorized_age conf base p))
              then result := (iper, alias_str) :: !result))
          aliases)
    (Driver.ipers base);
  List.rev !result

let search_firstname_with_aliases conf base query =
  Some.AliasCache.clear ();
  let direct_results = search_firstname_direct conf base query in
  let alias_results = search_firstname_aliases conf base query in

  List.iter
    (fun (iper, alias) -> Some.AliasCache.add_alias iper alias)
    alias_results;
  List.iter
    (fun (iper, alias) -> Some.AliasCache.add_alias iper alias)
    alias_results;

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
  let all_results = search_firstname_with_aliases conf base query in
  if (not opts.all) && not opts.exact then
    (List.map (fun r -> r.iper) all_results, [])
  else
    let fn_words = cut_words query in
    let exact = ref [] in
    let partial = ref [] in
    List.iter
      (fun result ->
        let ip = result.iper in
        let p = Driver.poi base ip in
        let fn_istr = Driver.get_first_name p in
        let fn = StringCache.get_cached base fn_istr in
        if fn <> "" then
          match result.match_source with
          | DirectMatch ->
              let fn_l = cut_words fn in
              if match_fn_lists fn_words fn_l opts then exact := ip :: !exact
              else if match_fn_lists fn_words fn_l { opts with all_in = false }
              then partial := ip :: !partial
          | FirstNameAlias _ -> exact := ip :: !exact)
      all_results;
    (List.rev !exact, List.rev !partial)

let search_surname_exact conf base query =
  let list, _name_inj =
    if query = "" then ([], fun x -> x)
    else
      Some.persons_of_fsname conf base Driver.base_strings_of_surname
        (Driver.spi_find (Driver.persons_of_surname base))
        Driver.get_surname query
  in
  let result = ref [] in
  let seen = Hashtbl.create 1000 in
  List.iter
    (fun (str, _, iperl) ->
      if Name.lower str = Name.lower query then
        List.iter
          (fun ip ->
            if not (Hashtbl.mem seen ip) then (
              Hashtbl.add seen ip ();
              let p = Driver.poi base ip in
              let sn = Driver.sou base (Driver.get_surname p) in
              if
                sn <> ""
                && (not (Driver.Istr.is_empty (Driver.get_first_name p)))
                && (not (Driver.Istr.is_empty (Driver.get_surname p)))
                && not
                     (Util.is_hide_names conf p
                     && not (Util.authorized_age conf base p))
              then result := ip :: !result))
          iperl)
    list;
  List.rev !result

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
          let sn_bearers = Some.search_surname conf base sn in
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

(* Fonction de recherche unifiée pour une variante *)
let search_one_variant_with_variants conf base query search_order =

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
          let ips = Some.search_surname conf base query in
          Logs.debug (fun k ->
              k "  Method Surname: %d results" (List.length ips));
          results := { !results with exact = !results.exact @ ips }
      | SurnameExact ->
          let ips = search_surname_exact conf base query in
          Logs.debug (fun k ->
              k "  Method SurnameExact: %d results" (List.length ips));
          results := { !results with exact = !results.exact @ ips }
      | FirstName ->
          let opts =
            {
              order = false;
              all = true;
              case = false;
              exact = false;
              all_in = true;
            }
          in
          let exact, partial =
            search_firstname_with_cache conf base query opts
          in
          List.iter
            (fun ip ->
              let p = Driver.poi base ip in
              let fn = StringCache.get_cached base (Driver.get_first_name p) in
              if fn <> "" then
                match Some.AliasCache.get_alias ip with
                | None ->
                    firstname_variants :=
                      Mutil.StrSet.add fn !firstname_variants
                | Some _ -> ())
            exact;
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
            }
      | DefaultSurname ->
          let ips = Some.search_surname conf base query in
          Logs.debug (fun k ->
              k "  Method DefaultSurname: %d results" (List.length ips));
          results := { !results with partial = !results.partial @ ips })
    search_order;
  let seen = DuplicateManager.create () in
  let exact = DuplicateManager.filter_new seen !results.exact in
  let partial = DuplicateManager.filter_new seen !results.partial in
  let spouse = DuplicateManager.filter_new seen !results.spouse in
  ({ exact; partial; spouse }, !firstname_variants)

let search_variants_two_pass conf base variants search_order =
  Logs.debug (fun k -> k "  search_variants_two_pass ");
  (* Séparer exact vs approximatif *)
  let exact_methods = [ Sosa; Key; SurnameExact; FirstName; FullName ] in
  let approx_methods = [ ApproxKey; PartialKey; Surname; DefaultSurname ] in

  (* Garder seulement les méthodes demandées *)
  let exact_methods =
    List.filter (fun m -> List.mem m search_order) exact_methods
  in
  let approx_methods =
    List.filter (fun m -> List.mem m search_order) approx_methods
  in

  let has_variants = List.length variants > 1 in

  (* Fonction pour tester un ensemble de méthodes *)
  let test_variants methods pass_name =
    let results = ref { exact = []; partial = []; spouse = [] } in
    let all_variants = ref Mutil.StrSet.empty in
    let found_any = ref false in

    List.iter
      (fun variant ->
        let r, fn_variants =
          search_one_variant_with_variants conf base variant methods
        in

        if r.exact <> [] || r.partial <> [] || r.spouse <> [] then (
          found_any := true;
          if has_variants then
            Logs.debug (fun k ->
                k "%s - Variant '%s': %d exact, %d partial, %d spouse" pass_name
                  variant (List.length r.exact) (List.length r.partial)
                  (List.length r.spouse));
          results :=
            {
              exact = !results.exact @ r.exact;
              partial = !results.partial @ r.partial;
              spouse = !results.spouse @ r.spouse;
            };
          all_variants := Mutil.StrSet.union !all_variants fn_variants))
      variants;

    (!results, !all_variants, !found_any)
  in

  (* PASSE 1: Tester d'abord les méthodes exactes *)
  let exact_results, exact_variants, found_exact =
    test_variants exact_methods "Pass 1 (exact)"
  in
  Logs.debug (fun k -> k "PASSE 1");

  if found_exact then (
    Logs.debug (fun k -> k "Found exact results, skipping approximative pass");
    (exact_results, exact_variants))
  else if approx_methods = [] then (
    Logs.debug (fun k -> k "No approximative methods available");
    (exact_results, exact_variants))
  else (
    Logs.debug (fun k -> k "PASSE 2");
    Logs.debug (fun k ->
        k "No exact results found, trying approximative methods");
    let approx_results, approx_variants, _found_approx =
      test_variants approx_methods "Pass 2 (approx)"
    in
    (approx_results, Mutil.StrSet.union exact_variants approx_variants))

let search conf base query search_order specify unknown =
  let variant_cache = Hashtbl.create 10 in
  let contains_apostrophe s =
    let rec check i =
      if i >= String.length s then false
      else
        match s.[i] with
        | '\'' -> true (* Apostrophe ASCII *)
        | '\xE2'
          when i + 2 < String.length s
               && s.[i + 1] = '\x80'
               && s.[i + 2] = '\x99' ->
            true (* U+2019 *)
        | '\xCA'
          when i + 1 < String.length s
               && (s.[i + 1] = '\xBC' || s.[i + 1] = '\xBB') ->
            true (* U+02BC, U+02BB *)
        | _ -> check (i + 1)
    in
    check 0
  in
  let variants =
    if contains_apostrophe query then ApostropheCache.get_variants query
    else [ query ]
  in
  let has_variants = List.length variants > 1 in
  if has_variants then
    Logs.debug (fun k ->
        k "Query: %s, Variants (%d): %s" query (List.length variants)
          (String.concat ", " variants));
  let all_results, collected_firstname_variants =
    if List.length variants = 1 then (
      Logs.debug (fun k -> k "No variants");
      (* Pas de variantes: logique normale avec cache *)
      let variant = List.hd variants in
      let r, fn_variants =
        try
          let cached = Hashtbl.find variant_cache variant in
          (cached, Mutil.StrSet.empty)
        with Not_found ->
          let res, variants =
            search_one_variant_with_variants conf base variant search_order
          in
          Hashtbl.add variant_cache variant res;
          (res, variants)
      in
      (r, fn_variants))
    else search_variants_two_pass conf base variants search_order
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
  Logs.debug (fun k ->
      k "Total results: %d ips (%d + %d + %d)" (List.length all_ips)
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
      Logs.debug (fun k ->
          k "Normalized parameters: fn=%s, sn=%s, pn=%s"
            (match fn with Some s -> "'" ^ s ^ "'" | None -> "None")
            (match sn with Some s -> "'" ^ s ^ "'" | None -> "None")
            (match pn with Some s -> "'" ^ s ^ "'" | None -> "None"));

      match (fn, sn, pn) with
      (* CAS 1: PRÉNOM seul *)
      | Some _, None, None ->
          Logs.debug (fun k -> k "Case: First name only");
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
      | None, Some _, None -> (
          Logs.debug (fun k -> k "Case: Surname only");
          let surname_groups = get_surname_groups () in
          Logs.debug (fun k ->
              k "Surname groups found: %d, Has variants: %b"
                (List.length surname_groups)
                has_variants);
          List.iter
            (fun (sn, persons) ->
              Logs.debug (fun k ->
                  k "  Group '%s': %d persons" sn (List.length persons)))
            surname_groups;
          match surname_groups with
          | [] -> assert false (* Cannot happen if all_ips is not empty *)
          | [ (surname, _) ] ->
              Logs.debug (fun k -> k "Single surname group: %s" surname);
              Some.search_surname_print conf base unknown surname
          | multiple ->
              Logs.debug (fun k ->
                  k "Multiple surname groups: %d" (List.length multiple));
              Some.print_multiple_display conf base query multiple)
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
      let order = [ Key; FullName; ApproxKey; PartialKey; DefaultSurname ] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | None, Some fn, None ->
      let fn =
        match String.rindex_opt fn '.' with
        | Some i -> String.sub fn 0 i
        | None -> fn
      in
      let order = [ FirstName ] in
      search conf base fn order specify unknown
  | Some pn, None, None -> (
      let order =
        [ Sosa; Key; FullName; ApproxKey; PartialKey; DefaultSurname ]
      in
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
            let order = [ SurnameExact; ApproxKey; DefaultSurname ] in
            search conf base sn order specify unknown
        | _ ->
            let order =
              [ Sosa; Key; FullName; ApproxKey; PartialKey; DefaultSurname ]
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
  | None, None, Some sn ->
      let order = [ SurnameExact; ApproxKey; DefaultSurname ] in
      search conf base sn order specify unknown
  | _ -> SrcfileDisplay.print_welcome conf base
