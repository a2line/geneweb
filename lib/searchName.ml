(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let max_results_per_category =
  1000 (* Limite par catégorie (exact/partial/spouse) *)

let _max_total_results = 2000 (* Limite totale *)
let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let trd3 (_, _, c) = c

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

let _contains_apostrophe s =
  let rec check i =
    if i >= String.length s then false
    else
      match s.[i] with
      | '\'' -> true (* Apostrophe ASCII *)
      | '\xE2'
        when i + 2 < String.length s && s.[i + 1] = '\x80' && s.[i + 2] = '\x99'
        ->
          true (* U+2019 *)
      | '\xCA'
        when i + 1 < String.length s
             && (s.[i + 1] = '\xBC' || s.[i + 1] = '\xBB') ->
          true (* U+02BC, U+02BB *)
      | _ -> check (i + 1)
  in
  check 0

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

(* Type unifié pour les résultats de recherche *)
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

(* Convertit une liste de persons en ipers *)
let persons_to_ipers persons = List.map Driver.get_iper persons

(* Recherche par sosa optimisée *)
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
  let lower fn_l =
    List.map (fun fn -> if opts.case then fn else Name.lower fn) fn_l
  in
  let equal fn1 fn2 =
    if opts.exact then String.compare fn1 fn2 = 0 else Mutil.contains fn2 fn1
  in
  let _list_equal equal fn1_l fn2_l =
    try List.length fn1_l = List.length fn2_l && List.for_all2 equal fn1_l fn2_l
    with Invalid_argument _ -> false
  in
  let rec is_subsequence l1 l2 =
    match (l1, l2) with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys ->
        if equal x y then is_subsequence xs ys else is_subsequence l1 ys
  in
  let fn_l = lower fn_l in
  let fn1_l = lower fn1_l in
  match (fn_l, opts.all, opts.order) with
  | [], _, _ -> true
  | [ fn ], _, _ when List.compare_length_with fn1_l 1 = 0 ->
      equal fn (List.hd fn1_l)
  | _, true, true -> is_subsequence fn_l fn1_l
  | _, true, false -> List.for_all (fun fn -> List.mem fn fn1_l) fn_l
  | _, false, _ ->
      List.exists (fun fn -> List.exists (fun fn1 -> equal fn fn1) fn1_l) fn_l

let search_for_multiple_fn conf base fn pl opts =
  (* Check if l1 is a contiguous sublist of l2 *)
  let fn_l = cut_words fn in
  List.fold_left
    (fun pl p ->
      if search_reject_p conf base p then pl
      else
        let fn1_l =
          Driver.get_first_name p |> Driver.sou base
          |> split_normalize opts.case
        in
        let fn2_l =
          Driver.get_public_name p |> Driver.sou base
          |> split_normalize opts.case
        in
        if match_fn_lists fn_l fn1_l opts || match_fn_lists fn_l fn2_l opts then
          p :: pl
        else pl)
    [] pl

(* Filtre les personnes selon les critères de prénom *)
let filter_by_firstname base fn_words opts persons =
  List.filter
    (fun p ->
      let fn = Driver.sou base (Driver.get_first_name p) in
      let fn_l = cut_words fn in
      fn <> "" && match_fn_lists fn_words fn_l opts)
    persons

(* Recherche de prénom optimisée avec cache *)
let search_firstname_with_cache conf base query opts =
  let fn_words = cut_words query in
  let save_env = conf.env in

  (* Recherche exacte *)
  let conf_exact =
    {
      conf with
      env =
        ("first_name", Adef.encoded query)
        :: ("exact_first_name", Adef.encoded "on")
        :: save_env;
    }
  in
  let exact_results, _ = AdvSearchOk.advanced_search conf_exact base max_int in
  let exact_filtered = filter_by_firstname base fn_words opts exact_results in

  (* Recherche non exacte *)
  let conf_fuzzy =
    {
      conf with
      env =
        ("first_name", Adef.encoded query)
        :: ("exact_first_name", Adef.encoded "off")
        :: save_env;
    }
  in
  let fuzzy_results, _ = AdvSearchOk.advanced_search conf_fuzzy base max_int in
  let fuzzy_filtered = filter_by_firstname base fn_words opts fuzzy_results in

  (* Retourner les ipers directement *)
  (persons_to_ipers exact_filtered, persons_to_ipers fuzzy_filtered)

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

  match persons with
  | [] -> { exact = []; partial = []; spouse = [] }
  | [ p ] -> { exact = [ Driver.get_iper p ]; partial = []; spouse = [] }
  | pl ->
      (* Créer les options pour la recherche *)
      let opts =
        {
          order = false;
          all = true;
          case = false;
          exact = false;
          all_in = true;
        }
      in
      (* Recherche avec all_in = true pour exact *)
      let exact = search_for_multiple_fn conf base fn pl opts in

      (* Recherche avec all_in = false pour partial *)
      let opts_partial = { opts with all_in = false } in
      let partial = search_for_multiple_fn conf base fn pl opts_partial in

      (* Recherche dans les conjoints si autorisé *)
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
          search_for_multiple_fn conf base fn spouses opts_partial
        else []
      in
      {
        exact = persons_to_ipers exact;
        partial = persons_to_ipers partial;
        spouse = persons_to_ipers spouse;
      }

(* Recherche par clé partielle optimisée *)
let search_partial_key conf base query =
  let pl = search_by_name conf base query in
  match pl with
  | [] ->
      (* Essayer recherche avancée *)
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
        (* Options corrigées *)
        let opts_exact =
          {
            order = false;
            all = true;
            case = false;
            exact = false;
            all_in = true;
          }
        in
        let opts_partial = { opts_exact with all_in = false } in
        let exact = search_for_multiple_fn conf base fn persons opts_exact in
        let partial =
          search_for_multiple_fn conf base fn persons opts_partial
        in
        {
          exact = persons_to_ipers exact;
          partial = persons_to_ipers partial;
          spouse = [];
        }
  | _ -> { exact = persons_to_ipers pl; partial = []; spouse = [] }

module ApostropheCache = struct
  let cache = Hashtbl.create 100

  let get_variants s =
    try Hashtbl.find cache s
    with Not_found ->
      let variants = generate_apostrophe_variants s in
      Hashtbl.add cache s variants;
      variants
end

(* Fonction de recherche d'une variante avec limites *)
let _search_one_variant_refactored_limited conf base query search_order
    ~ignore_limits =
  let results = ref { exact = []; partial = []; spouse = [] } in
  let counts = ref (0, 0, 0) in

  List.iter
    (function
      | Sosa ->
          let ips = search_sosa_opt conf base query in
          let ips =
            if ignore_limits then ips
            else Mutil.list_limit max_results_per_category ips
          in
          results := { !results with exact = !results.exact @ ips };
          counts := (fst3 !counts + List.length ips, snd3 !counts, trd3 !counts)
      | Key ->
          let ips = search_key_opt conf base query in
          let ips =
            if ignore_limits then ips
            else Mutil.list_limit max_results_per_category ips
          in
          results := { !results with exact = !results.exact @ ips };
          counts := (fst3 !counts + List.length ips, snd3 !counts, trd3 !counts)
      | Surname ->
          let ips = Some.search_surname conf base query in
          let ips =
            if ignore_limits then ips
            else Mutil.list_limit max_results_per_category ips
          in
          results := { !results with exact = !results.exact @ ips };
          counts := (fst3 !counts + List.length ips, snd3 !counts, trd3 !counts)
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
          let exact, partial =
            if ignore_limits then (exact, partial)
            else
              let remaining_exact = max_results_per_category - fst3 !counts in
              let remaining_partial = max_results_per_category - snd3 !counts in
              ( Mutil.list_limit remaining_exact exact,
                Mutil.list_limit remaining_partial partial )
          in
          results :=
            {
              !results with
              exact = !results.exact @ exact;
              partial = !results.partial @ partial;
            };
          counts :=
            ( fst3 !counts + List.length exact,
              snd3 !counts + List.length partial,
              trd3 !counts )
      | _ -> ())
    search_order;

  let seen = DuplicateManager.create () in
  let exact = DuplicateManager.filter_new seen !results.exact in
  let partial = DuplicateManager.filter_new seen !results.partial in
  let spouse = DuplicateManager.filter_new seen !results.spouse in
  ({ exact; partial; spouse }, !counts)

(* Fonction de recherche unifiée pour une variante *)
let search_one_variant_refactored conf base query search_order =
  let results = ref { exact = []; partial = []; spouse = [] } in

  List.iter
    (function
      | Sosa ->
          let ips = search_sosa_opt conf base query in
          results := { !results with exact = !results.exact @ ips }
      | Key ->
          let ips = search_key_opt conf base query in
          results := { !results with exact = !results.exact @ ips }
      | Surname ->
          let ips = Some.search_surname conf base query in
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
          results :=
            {
              exact = !results.exact @ exact;
              partial = !results.partial @ partial;
              spouse = !results.spouse;
            }
      | FullName ->
          (* Extraire fn et sn intelligemment *)
          let fn, sn =
            match (p_getenv conf.env "p", p_getenv conf.env "n") with
            | Some fn, Some sn when fn <> "" && sn <> "" -> (fn, sn)
            | _ -> (
                (* Diviser query au dernier espace *)
                match String.rindex_opt query ' ' with
                | Some i ->
                    ( String.sub query 0 i,
                      String.sub query (i + 1) (String.length query - i - 1) )
                | None -> ("", query))
          in
          let r = search_fullname conf base fn sn in
          results :=
            {
              exact = !results.exact @ r.exact;
              partial = !results.partial @ r.partial;
              spouse = !results.spouse @ r.spouse;
            }
      | PartialKey ->
          let r = search_partial_key conf base query in
          results :=
            {
              exact = !results.exact @ r.exact;
              partial = !results.partial @ r.partial;
              spouse = !results.spouse @ r.spouse;
            }
      | ApproxKey ->
          let pl = search_approx_key conf base query in
          let ips = persons_to_ipers pl in
          results := { !results with exact = !results.exact @ ips }
      | DefaultSurname ->
          let ips = Some.search_surname conf base query in
          results := { !results with exact = !results.exact @ ips })
    search_order;

  (* Éliminer les duplicatas entre catégories *)
  let seen = DuplicateManager.create () in
  let exact = DuplicateManager.filter_new seen !results.exact in
  let partial = DuplicateManager.filter_new seen !results.partial in
  let spouse = DuplicateManager.filter_new seen !results.spouse in

  { exact; partial; spouse }

(* lib/searchName.ml - Fonction search CORRIGÉE pour les paramètres vides *)

let search conf base query search_order specify unknown =
  (* Utiliser le cache pour les variantes d'apostrophes *)
  let variants = ApostropheCache.get_variants query in
  let has_variants = List.length variants > 1 in

  (* DEBUG *)
  if conf.wizard then (
    Printf.eprintf "[DEBUG] Query: %s\n" query;
    Printf.eprintf "[DEBUG] Variants (%d): %s\n" (List.length variants)
      (String.concat ", " variants));

  (* Rechercher avec toutes les variantes *)
  let all_results =
    List.fold_left
      (fun acc variant ->
        let r = search_one_variant_refactored conf base variant search_order in

        if conf.wizard then
          Printf.eprintf
            "[DEBUG] Variant '%s': %d exact, %d partial, %d spouse\n" variant
            (List.length r.exact) (List.length r.partial) (List.length r.spouse);

        {
          exact = acc.exact @ r.exact;
          partial = acc.partial @ r.partial;
          spouse = acc.spouse @ r.spouse;
        })
      { exact = []; partial = []; spouse = [] }
      variants
  in

  (* Éliminer les duplicatas globaux *)
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

  (* DEBUG *)
  if conf.wizard then
    Printf.eprintf "[DEBUG] Total results: %d ips\n" (List.length all_ips);

  (* Gérer l'affichage selon les résultats *)
  match all_ips with
  | [] -> SrcfileDisplay.print_welcome conf base
  | [ ip ] ->
      record_visited conf ip;
      Perso.print conf base (Driver.poi base ip)
  | _ -> (
      let pl1 = List.map (Driver.poi base) final_results.exact in
      let pl2 = List.map (Driver.poi base) final_results.partial in
      let pl3 = List.map (Driver.poi base) final_results.spouse in

      (* CORRECTION : Traiter les chaînes vides comme None *)
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

      (* DEBUG *)
      if conf.wizard then
        Printf.eprintf "[DEBUG] Normalized parameters: fn=%s, sn=%s, pn=%s\n"
          (match fn with Some s -> "'" ^ s ^ "'" | None -> "None")
          (match sn with Some s -> "'" ^ s ^ "'" | None -> "None")
          (match pn with Some s -> "'" ^ s ^ "'" | None -> "None");

      (* Logique de décision pour l'affichage *)
      match (fn, sn, pn) with
      (* CAS 1: Recherche par PRÉNOM seul *)
      | Some _, None, None ->
          if conf.wizard then Printf.eprintf "[DEBUG] Case: First name only\n";
          let str = List.fold_left (fun acc p ->
            Mutil.StrSet.add (Driver.sou base @@ Driver.get_first_name p)
            acc) Mutil.StrSet.empty pl1
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
      (* CAS 2: Recherche par NOM DE FAMILLE seul *)
      | None, Some _, None -> (
          if conf.wizard then Printf.eprintf "[DEBUG] Case: Surname only\n";

          (* Grouper par nom de famille pour voir les variantes *)
          let surname_groups = group_by_surname base all_ips in

          (* DEBUG *)
          if conf.wizard then (
            Printf.eprintf "[DEBUG] Surname groups found: %d\n"
              (List.length surname_groups);
            List.iter
              (fun (sn, persons) ->
                Printf.eprintf "[DEBUG]   Group '%s': %d persons\n" sn
                  (List.length persons))
              surname_groups;
            Printf.eprintf "[DEBUG] Has variants: %b\n" has_variants);

          (* Décider selon le nombre de groupes et la présence de variantes *)
          match surname_groups with
          | [] ->
              if conf.wizard then
                Printf.eprintf "[DEBUG] No groups (should not happen)\n";
              specify conf base query pl1 pl2 pl3
          | [ (surname, _) ] ->
              (* Un seul groupe de nom *)
              if conf.wizard then
                Printf.eprintf "[DEBUG] Single surname group: %s\n" surname;
              Some.search_surname_print conf base unknown surname
          | multiple ->
              (* Plusieurs groupes de noms *)
              if conf.wizard then
                Printf.eprintf "[DEBUG] Multiple surname groups: %d\n"
                  (List.length multiple);
              if has_variants then (
                if conf.wizard then
                  Printf.eprintf "[DEBUG] Using print_multiple_display\n";
                Some.print_multiple_display conf base query multiple)
              else (
                if conf.wizard then
                  Printf.eprintf "[DEBUG] Using specify (no variants)\n";
                specify conf base query pl1 pl2 pl3))
      (* CAS 3: Recherche avec PN (format prénom/nom/occ) *)
      | None, None, Some pn_value -> (
          if conf.wizard then Printf.eprintf "[DEBUG] Case: PN format\n";
          let i = try String.index pn_value '/' with Not_found -> -1 in

          if i = -1 then specify conf base query pl1 pl2 pl3
          else
            (* Format avec slash *)
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
                let str = List.fold_left (fun acc p ->
                  Mutil.StrSet.add (Driver.sou base @@ Driver.get_first_name p)
                  acc) Mutil.StrSet.empty pl1
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
                let surname_groups = group_by_surname base all_ips in

                match surname_groups with
                | [] -> specify conf base query pl1 pl2 pl3
                | [ (surname, _) ] ->
                    Some.search_surname_print conf base unknown surname
                | multiple ->
                    if has_variants then
                      Some.print_multiple_display conf base query multiple
                    else specify conf base query pl1 pl2 pl3)
            (* Autres cas avec pn *)
            | _ -> specify conf base query pl1 pl2 pl3)
      (* CAS 4: Recherche PRÉNOM + NOM *)
      | Some _, Some _, None ->
          if conf.wizard then
            Printf.eprintf "[DEBUG] Case: First name + Surname\n";
          specify conf base query pl1 pl2 pl3
      (* CAS PAR DÉFAUT *)
      | _ ->
          if conf.wizard then Printf.eprintf "[DEBUG] Case: Default\n";
          specify conf base query pl1 pl2 pl3)

let _search conf base an search_order specify unknown =
  let test label = p_getenv conf.env label = Some "on" in
  let test_not label = p_getenv conf.env label = Some "off" in
  let opts =
    {
      exact = not (test_not "p_exact");
      case = test "p_case";
      order = test "p_order";
      all = not (test_not "p_all");
      all_in = false;
    }
  in
  (* Generate apostrophe variants BEFORE any processing *)
  let fn =
    match p_getenv conf.env "p" with
    | Some "" | None -> None
    | Some fn -> Some (Name.lower fn)
  in
  let sn =
    match p_getenv conf.env "n" with
    | Some "" | None -> None
    | Some sn -> Some (Name.lower sn)
  in
  let pn =
    match p_getenv conf.env "pn" with
    | Some "" | None -> None
    | Some pn -> Some (Name.lower pn)
  in
  let search_variants = generate_apostrophe_variants an in
  let surname_groups = Hashtbl.create 10 in
  let seen_iper = Hashtbl.create 40 in
  let add_person_to_groups p =
    let ip = Driver.get_iper p in
    if not (Hashtbl.mem seen_iper ip) then (
      Hashtbl.add seen_iper ip ();
      let sn = Driver.sou base (Driver.get_surname p) in
      let persons = try Hashtbl.find surname_groups sn with Not_found -> [] in
      Hashtbl.replace surname_groups sn (p :: persons))
  in
  let search_one_variant an search_order =
    let rec loop (pl1, pl2, pl3) l =
      match l with
      | [] -> (pl1, pl2, pl3)
      | Sosa :: l -> (
          match search_by_sosa conf base an with
          | None -> loop (pl1, pl2, pl3) l
          | Some p -> (Driver.get_iper p :: pl1, pl2, pl3))
      | Key :: l -> (
          match search_by_key conf base an with
          | None -> loop (pl1, pl2, pl3) l
          | Some p -> (Driver.get_iper p :: pl1, pl2, pl3))
      | Surname :: l -> (
          let pl = Some.search_surname conf base an in
          match pl with
          | [] -> loop (pl1, pl2, pl3) l
          | _ -> (pl @ pl1, pl2, pl3))
      | FirstName :: l -> (
          let fn_l = cut_words an in
          let save_env = conf.env in
          let conf =
            {
              conf with
              env =
                ("first_name", Adef.encoded an)
                :: ("exact_first_name", Adef.encoded "on")
                :: save_env;
            }
          in
          (* find all bearers of sn with all exact = "on" fn using advanced_search *)
          let pl1, _len = AdvSearchOk.advanced_search conf base max_int in
          (* filter out with match_fn_list *)
          let pl1 =
            List.fold_left
              (fun acc p ->
                let fn1 = Driver.sou base (Driver.get_first_name p) in
                let fn1_l = cut_words fn1 in
                if fn1 = "" then acc
                else if match_fn_lists fn_l fn1_l opts then p :: acc
                else acc)
              [] pl1
          in
          let conf =
            {
              conf with
              env =
                ("first_name", Adef.encoded an)
                :: ("exact_first_name", Adef.encoded "off")
                :: save_env;
            }
          in
          (* find additional bearers of sn with with exact = "off" fn using advanced_search *)
          let pl1_ht = Hashtbl.create 40 in
          List.iter (fun p -> Hashtbl.add pl1_ht (Driver.get_iper p) "") pl1;
          let pl2, _len = AdvSearchOk.advanced_search conf base max_int in
          (* filter out with match_fn_lists *)
          let pl2 =
            List.fold_left
              (fun acc p ->
                let fn1 = Driver.sou base (Driver.get_first_name p) in
                let fn1_l = cut_words fn1 in
                if fn1 = "" then acc
                else if match_fn_lists fn_l fn1_l opts then p :: acc
                else acc)
              [] pl2
          in
          (* remove from pl2 persons already in pl1 *)
          let pl2 =
            List.fold_left
              (fun acc p ->
                if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc else p :: acc)
              [] pl2
          in
          (* split pl1 into exact matches (pl1) and partial match (pl3) *)
          let pl1, pl3 =
            List.fold_left
              (fun (acc1, acc3) p ->
                let fn1 = Driver.sou base (Driver.get_public_name p) in
                let fn1_l = cut_words fn1 in
                if fn1 = "" then (p :: acc1, acc3)
                else if match_fn_lists fn_l fn1_l opts then (acc1, p :: acc3)
                else (p :: acc1, acc3))
              ([], []) pl1
          in
          let pl1_ht = Hashtbl.create 40 in
          List.iter (fun p -> Hashtbl.add pl1_ht (Driver.get_iper p) "") pl1;
          (* remove from pl2 persons already in pl1 *)
          let pl2 =
            List.fold_left
              (fun acc p ->
                if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc else p :: acc)
              [] pl2
          in
          (* remove from pl3 persons already in pl1 *)
          let pl3 =
            List.fold_left
              (fun acc p ->
                if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc else p :: acc)
              [] pl3
          in
          let pl2_ht = Hashtbl.create 40 in
          List.iter (fun p -> Hashtbl.add pl2_ht (Driver.get_iper p) "") pl2;
          (* remove from pl3 persons already in pl2 *)
          let pl3 =
            List.fold_left
              (fun acc p ->
                if Hashtbl.mem pl2_ht (Driver.get_iper p) then acc else p :: acc)
              [] pl3
          in
          let pl1 = List.map (fun p -> Driver.get_iper p) pl1 in
          let pl2 = List.map (fun p -> Driver.get_iper p) pl2 in
          let pl3 = List.map (fun p -> Driver.get_iper p) pl3 in
          match (pl1, pl2, pl3) with
          | [], [], [] -> loop (pl1, pl2, pl3) l
          | _ -> (pl1, pl2, pl3))
      | FullName :: l -> (
          let fn, sn =
            match (fn, sn) with
            | None, None -> (
                (* we assume pn = fn1 fn2 sn. For other cases, use fn, sn explicitely *)
                (* TODO check for particles and cut before particle *)
                (* see if    Name.abbrev (Name.lower sn)    is Ok *)
                (* or use split_normalize here? *)
                let an = Name.lower an in
                match String.rindex_opt an ' ' with
                | Some i ->
                    ( String.sub an 0 i,
                      String.sub an (i + 1) (String.length an - i - 1) )
                | _ -> ("", an))
            | Some fn, Some sn -> (fn, sn)
            | None, Some sn -> ("", sn)
            | Some fn, None -> (fn, "")
          in
          let conf =
            {
              conf with
              env =
                ("surname", Adef.encoded sn)
                :: ("exact_surname", Adef.encoded "on")
                :: conf.env;
            }
          in
          (* find all bearers of sn using advanced_search *)
          let list, _len = AdvSearchOk.advanced_search conf base max_int in
          match list with
          | [] -> loop (pl1, pl2, pl3) l
          | [ p ] -> (Driver.get_iper p :: pl1, pl2, pl3)
          | pl ->
              (* check first_names or public_names in list of persons *)
              let opts1 = { opts with all_in = true } in
              let pl1 = search_for_multiple_fn conf base fn pl opts1 in
              let opts2 = { opts with all_in = false } in
              let pl2 = search_for_multiple_fn conf base fn pl opts2 in
              let pl1_ht = Hashtbl.create 40 in
              List.iter (fun p -> Hashtbl.add pl1_ht (Driver.get_iper p) "") pl1;
              let pl2 =
                List.fold_left
                  (fun acc p ->
                    if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc
                    else p :: acc)
                  [] pl2
              in
              let get_spouse iper ifam =
                let f = Driver.foi base ifam in
                if iper = Driver.get_father f then
                  Driver.poi base (Driver.get_mother f)
                else Driver.poi base (Driver.get_father f)
              in
              (* find bearers of surname *)
              let find_pl3 =
                not
                  (List.assoc_opt "public_name_as_fn" conf.base_env = Some "no")
              in
              let pl3 =
                if find_pl3 then Some.search_surname conf base sn else []
              in
              let pl3 =
                List.fold_left
                  (fun acc ip ->
                    Array.fold_left
                      (fun acc ifam -> get_spouse ip ifam :: acc)
                      acc
                      (Driver.get_family (Driver.poi base ip)))
                  [] pl3
              in
              let pl3 = search_for_multiple_fn conf base fn pl3 opts in
              let pl1 = List.map (fun p -> Driver.get_iper p) pl1 in
              let pl2 = List.map (fun p -> Driver.get_iper p) pl2 in
              let pl3 = List.map (fun p -> Driver.get_iper p) pl3 in
              (pl1, pl2, pl3))
      | ApproxKey :: l -> (
          let pl = search_approx_key conf base an in
          let pl = List.map (fun p -> Driver.get_iper p) pl in
          match pl with
          | [] -> loop (pl1, pl2, pl3) l
          | [ p ] -> (p :: pl1, pl2, pl3)
          | pl -> (pl @ pl1, pl2, pl3))
      | PartialKey :: l -> (
          let pl = search_by_name conf base an in
          let pl4 = List.map (fun p -> Driver.get_iper p) pl in
          match pl4 with
          | [] -> (
              (* try advanced search *)
              (* TODO use split_normalize here? why only split on the first ' '? *)
              let n1 = Name.abbrev (Name.lower an) in
              let fn, sn =
                match String.index_opt n1 ' ' with
                | Some i ->
                    ( String.sub n1 0 i,
                      String.sub n1 (i + 1) (String.length n1 - i - 1) )
                | _ -> ("", n1)
              in
              let conf =
                { conf with env = ("surname", Adef.encoded sn) :: conf.env }
              in
              let p_of_sn_l, _len =
                AdvSearchOk.advanced_search conf base max_int
              in
              match p_of_sn_l with
              | [] -> loop (pl1, pl2, pl3) l
              | [ p ] -> (Driver.get_iper p :: pl1, pl2, pl3)
              | pl -> (
                  let opts1 = { opts with all_in = true } in
                  let pl1 = search_for_multiple_fn conf base fn pl opts1 in
                  let opts2 = { opts with all_in = false } in
                  let pl2 = search_for_multiple_fn conf base fn pl opts2 in
                  let pl2 =
                    List.fold_left
                      (fun acc p -> if List.mem p pl1 then acc else p :: acc)
                      [] pl2
                  in
                  let pl1 = List.map (fun p -> Driver.get_iper p) pl1 in
                  let pl2 = List.map (fun p -> Driver.get_iper p) pl2 in
                  match pl1 with
                  | [] -> loop (pl1, pl2, pl3) l
                  | [ p ] -> (p :: pl1, pl2, pl3)
                  | _ -> (pl1, pl2, pl3)))
          | [ p ] -> (p :: pl1, pl2, pl3)
          | pl -> (pl @ pl1, pl2, pl3))
      | DefaultSurname :: _ ->
          let pl = Some.search_surname conf base an in
          (pl @ pl1, pl2, pl3)
    in
    loop ([], [], []) search_order
  in

  let pl1, pl2, pl3 =
    let rec search_all_variants (pl1, pl2, pl3) variants =
      match variants with
      | [] -> (pl1, pl2, pl3)
      | v :: variants ->
          let pl11, pl21, pl31 = search_one_variant v search_order in
          search_all_variants (pl1 @ pl11, pl2 @ pl21, pl3 @ pl31) variants
    in
    search_all_variants ([], [], []) search_variants
  in

  List.iter (fun ip -> add_person_to_groups (Driver.poi base ip)) pl1;
  List.iter (fun ip -> add_person_to_groups (Driver.poi base ip)) pl2;
  List.iter (fun ip -> add_person_to_groups (Driver.poi base ip)) pl3;
  let surnames =
    Hashtbl.fold (fun sn pl acc -> (sn, List.rev pl) :: acc) surname_groups []
  in

  match (pl1, pl2, pl3) with
  | [], [], [] -> SrcfileDisplay.print_welcome conf base
  | [ ip ], [], [] | [], [ ip ], [] | [], [], [ ip ] ->
      record_visited conf ip;
      Perso.print conf base (Driver.poi base ip)
  | pl1, pl2, pl3 -> (
      let pl1 = List.map (fun ip -> Driver.poi base ip) pl1 in
      let pl2 = List.map (fun ip -> Driver.poi base ip) pl2 in
      let pl3 = List.map (fun ip -> Driver.poi base ip) pl3 in
      match (fn, sn, pn) with
      | Some _fn, None, None -> specify conf base an pl1 pl2 pl3
      | None, Some _sn, None -> (
          match surnames with
          | [ (surname, _persons) ] ->
              Some.search_surname_print conf base unknown surname
          | multiple_surnames ->
              let title _ =
                Output.printf conf {|%s %s|}
                  (Util.escape_html an :> string)
                  (transl conf "specify")
              in
              Hutil.header conf title;
              let sorted_surnames =
                List.sort
                  (fun (sn1, _) (sn2, _) -> String.compare sn1 sn2)
                  multiple_surnames
              in
              List.iter
                (fun (sn, persons) ->
                  Output.printf conf
                    {|<h3 class="mt-3"><a href="%sm=N&v=%s"><strong>%s</strong></a> (%d)</h3>|}
                    (commd conf :> string)
                    (Mutil.encode sn :> string)
                    (Util.escape_html sn :> string)
                    (List.length persons);
                  Output.print_sstring conf "<ul>\n";
                  let sorted_persons =
                    List.sort
                      (fun p1 p2 ->
                        match
                          ( Date.od_of_cdate (Driver.get_birth p1),
                            Date.od_of_cdate (Driver.get_birth p2) )
                        with
                        | Some d1, Some d2 -> Date.compare_date d1 d2
                        | None, Some _ -> 1
                        | Some _, None -> -1
                        | None, None ->
                            Gutil.alphabetic_order
                              (Driver.p_first_name base p1)
                              (Driver.p_first_name base p2))
                      persons
                  in
                  List.iter
                    (fun p ->
                      Output.print_sstring conf "<li>";
                      Update.print_person_parents_and_spouses conf base p;
                      Output.print_sstring conf "</li>\n")
                    sorted_persons;
                  Output.print_sstring conf "</ul>\n")
                sorted_surnames;
              Hutil.trailer conf)
      | _ -> specify conf base an pl1 pl2 pl3)

(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)

(* ************************************************************************ *)

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
            let order = [ Surname; ApproxKey; DefaultSurname ] in
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
      let order = [ Surname; ApproxKey; DefaultSurname ] in
      search conf base sn order specify unknown
  | _ -> SrcfileDisplay.print_welcome conf base
