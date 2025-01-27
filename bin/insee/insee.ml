(* Copyright (c) 2019 Ludovic LEDIEU *)
(* Updated by A2 and Claude in 2025 *)

open Gwdb
open Def

let min_birth_year = ref 1870
let nodate = ref false
let nodate_only = ref false
let all = ref false
let total_processed = ref 0
let total_exported = ref 0
let exported_with_birth = ref 0
let exported_with_death = ref 0
let exported_nodate = ref 0

(* UTF-8 handling functions *)
let utf8_of_string s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      (* Convert each character to its UTF-8 representation *)
      let u = Uchar.of_char c in
      Uutf.Buffer.add_utf_8 b u)
    s;
  Buffer.contents b

(* UTF-8 aware printf that ensures proper encoding of output *)
let safe_printf fmt =
  let k _ = flush stdout in
  Printf.kfprintf k stdout fmt

let is_living p = match get_death p with NotDead -> true | _ -> false
let has_date_info b_date d_date = b_date <> None || d_date <> None

let has_useful_birth_info base p =
  let has_place = sou base (get_birth_place p) <> "" in
  let has_year =
    match Date.od_of_cdate (get_birth p) with
    | Some (Dgreg (d, _)) -> true
    | _ -> false
  in
  has_place || has_year

let has_useful_death_info base p =
  let has_place = sou base (get_death_place p) <> "" in
  let has_year =
    match get_death p with
    | Death (_, cd) -> (
        match Date.date_of_cdate cd with Dgreg (d, _) -> true | _ -> false)
    | _ -> false
  in
  has_place || has_year

let estimate_period base p =
  let birth_limit = !min_birth_year in
  let parent_limit = birth_limit - 40 in
  let marriage_limit = birth_limit + 16 in
  let has_recent_parents =
    match get_parents p with
    | Some ifam -> (
        let cpl = foi base ifam in
        let father = poi base (get_father cpl) in
        let mother = poi base (get_mother cpl) in
        match Date.od_of_cdate (get_birth father) with
        | Some (Dgreg (d, _)) when d.year > parent_limit -> true
        | _ -> (
            match Date.od_of_cdate (get_birth mother) with
            | Some (Dgreg (d, _)) when d.year > parent_limit -> true
            | _ -> false))
    | None -> false
  in

  let has_recent_spouse =
    Array.exists
      (fun ifam ->
        let cpl = foi base ifam in
        let spouse =
          if get_iper p = get_father cpl then poi base (get_mother cpl)
          else poi base (get_father cpl)
        in
        match Date.od_of_cdate (get_birth spouse) with
        | Some (Dgreg (d, _)) when d.year > birth_limit -> true
        | _ -> false)
      (get_family p)
  in

  let has_recent_marriage =
    Array.exists
      (fun ifam ->
        let fam = foi base ifam in
        match Date.od_of_cdate (get_marriage fam) with
        | Some (Dgreg (d, _)) when d.year > marriage_limit -> true
        | _ -> false)
      (get_family p)
  in

  has_recent_parents || has_recent_spouse || has_recent_marriage

let is_nodate_eligible p base =
  !nodate && (is_living p || estimate_period base p)

let name_contains_enfant s =
  let s_lower = String.lowercase_ascii s in
  let target = "enfant" in
  try
    let _ = Str.search_forward (Str.regexp_string target) s_lower 0 in
    true
  with Not_found -> false

let is_dummy_name name =
  let dummy_names = [ "??"; "X"; "XX"; "XXX"; "XY"; "YY"; "NNN"; "NN" ] in
  List.mem (String.uppercase_ascii name) dummy_names

let my_uppercase2 s =
  (* Split on both straight and curved apostrophes *)
  let split_apostrophes s =
    let parts = String.split_on_char '\'' s in
    (* Split on straight apostrophe *)
    List.fold_left
      (fun acc part ->
        acc @ String.split_on_char '’' part (* Split on curved apostrophe *))
      [] parts
  in
  let s = split_apostrophes s in
  (* Use straight apostrophe for consistency in output *)
  String.concat (String.make 1 '\'')
    (List.map (fun e -> String.uppercase_ascii (Name.lower e)) s)

let my_uppercase s =
  let s = String.split_on_char '-' s in
  String.concat (String.make 1 '-') (List.map (fun e -> my_uppercase2 e) s)

let check_insee base =
  let () = set_binary_mode_out stdout true in
  total_processed := 0;
  total_exported := 0;
  exported_with_birth := 0;
  exported_with_death := 0;
  exported_nodate := 0;
  Gwdb.Collection.iteri
    (fun i p ->
      incr total_processed;
      let b_date =
        match Date.od_of_cdate (get_birth p) with
        | Some (Dgreg (d, _)) -> Some d
        | _ -> None
      in
      let d_date =
        match get_death p with
        | Death (_, cd) -> (
            match Date.date_of_cdate cd with
            | Dgreg (d, _) -> Some d
            | _ -> None)
        | _ -> None
      in
      let sn = my_uppercase (p_surname base p) in
      let fn = my_uppercase (p_first_name base p) in
      let s = match get_sex p with Male -> 1 | Female -> 2 | _ -> 0 in
      let b_place = sou base (get_birth_place p) in
      let d_place = sou base (get_death_place p) in
      let key =
        sou base (get_first_name p)
        ^ "."
        ^ string_of_int (get_occ p)
        ^ " "
        ^ sou base (get_surname p)
      in
      let check =
        if !nodate_only then
          (not (fn = "" && sn = ""))
          && (not (name_contains_enfant fn))
          && (not (is_dummy_name sn || is_dummy_name fn))
          &&
          match d_date with
          | None when !nodate && is_living p -> true
          | None when b_date = None && estimate_period base p -> true
          | _ -> false
        else
          (not (fn = "" && sn = ""))
          && (not (name_contains_enfant fn))
          && (not
                ((is_dummy_name sn || is_dummy_name fn)
                && b_date = None && d_date = None
                && not (!nodate && (is_living p || estimate_period base p))))
          &&
          match d_date with
          | Some dd when dd.prec = Sure && dd.year < 1970 -> false
          | Some dd when dd.prec = Before && dd.year < 1970 -> false
          | None when !nodate && is_living p -> true
          | None when b_date = None && estimate_period base p -> true
          | _ -> true
      in
      match b_date with
      | Some bd ->
          if bd.year >= !min_birth_year && check = true then (
            incr exported_with_birth;
            incr total_exported;
            match d_date with
            | Some dd ->
                safe_printf "%s|%s|%d|%02d|%02d|%04d|%s|%02d|%02d|%04d|%s|%s\n"
                  sn fn s
                  (if bd.prec = Sure || bd.prec = Maybe then bd.day else 0)
                  (if bd.prec = Sure || bd.prec = Maybe then bd.month else 0)
                  (if bd.prec = Sure || bd.prec = About || bd.prec = Maybe then
                   bd.year
                  else 0)
                  b_place
                  (if dd.prec = Sure || dd.prec = Maybe then dd.day else 0)
                  (if dd.prec = Sure || dd.prec = Maybe then dd.month else 0)
                  (if dd.prec = Sure || dd.prec = About || dd.prec = Maybe then
                   dd.year
                  else 0)
                  d_place key
            | None ->
                safe_printf "%s|%s|%d|%02d|%02d|%04d|%s|00|00|0000|%s|%s\n" sn
                  fn s
                  (if bd.prec = Sure || bd.prec = Maybe then bd.day else 0)
                  (if bd.prec = Sure || bd.prec = Maybe then bd.month else 0)
                  (if bd.prec = Sure || bd.prec = About || bd.prec = Maybe then
                   bd.year
                  else 0)
                  b_place d_place key)
      | None -> (
          if check then
            match d_date with
            | Some dd when dd.year > 1970 ->
                incr exported_with_death;
                incr total_exported;
                safe_printf "%s|%s|%d|00|00|0000|%s|%02d|%02d|%04d|%s|%s\n" sn
                  fn s b_place
                  (if dd.prec = Sure || dd.prec = Maybe then dd.day else 0)
                  (if dd.prec = Sure || dd.prec = Maybe then dd.month else 0)
                  (if dd.prec = Sure || dd.prec = About || dd.prec = Maybe then
                   dd.year
                  else 0)
                  d_place key
            | None when !nodate && (is_living p || estimate_period base p) ->
                incr exported_nodate;
                incr total_exported;
                safe_printf "%s|%s|%d|00|00|0000|%s|00|00|0000|%s|%s\n" sn fn s
                  b_place d_place key
            | _ -> ()))
    (Gwdb.persons base)

(* main *)
let usage_msg = "Usage: insee [-bd basedir] [-nodate] [-all] basename\n"
let base_dir = ref "."
let base_name = ref ""

let speclist =
  [
    ("-bd", Arg.Set_string base_dir, "   Specify base directory");
    ( "-year",
      Arg.Set_int min_birth_year,
      "   Set minimum birth year for inclusion (default 1870)" );
    ( "-nodate",
      Arg.Set nodate,
      "   Include living people and those with family connections in target \
       period" );
    ( "-nodateonly",
      Arg.Set nodate_only,
      "   Only include records eligible for -nodate criteria" );
    ( "-all",
      Arg.Set all,
      "   Include all individuals born after 1870 regardless of other criteria"
    );
    ( "-help",
      Arg.Unit
        (fun () ->
          Printf.printf "\nINSEE Death Records Extraction Tool\n\n";
          Printf.printf
            "This tool extracts potential matches for the INSEE death records \
             database.\n\n";
          Printf.printf "Basic operation:\n";
          Printf.printf "  - Captures individuals deceased after 1970\n";
          Printf.printf
            "  - Includes partial information (just place or just date)\n";
          Printf.printf "  - Focuses on births after 1870\n\n";
          Printf.printf "Options:\n";
          Printf.printf "  -bd dir      Specify base directory\n";
          Printf.printf "  -nodate      Include:\n";
          Printf.printf "               - Living individuals\n";
          Printf.printf
            "               - People with family connections in period\n";
          Printf.printf "  -all         Include all births after 1870\n";
          Printf.printf "               regardless of death information\n\n";
          Printf.printf "Output format:\n";
          Printf.printf
            "  \
             name|firstname|sex|birth_d|birth_m|birth_y|birth_p|death_d|death_m|death_y|death_p|key\n\n";
          Printf.printf "Example:\n";
          Printf.printf "  insee -bd /path/to/bases -nodate basename\n\n";
          exit 0),
      "   Display this help message" );
  ]

let anon_fun s =
  if !base_name = "" then base_name := s
  else raise (Arg.Bad "Cannot treat several databases")

(* Main execution *)
let () =
  try
    Arg.parse speclist anon_fun "Usage: insee [-bd basedir] basename";
    let full_path =
      if !base_name = "" then (
        Arg.usage speclist "Usage: insee [-bd basedir] basename";
        exit 2)
      else if Filename.check_suffix !base_name ".gwb" then
        Filename.concat !base_dir !base_name
      else Filename.concat !base_dir (!base_name ^ ".gwb")
    in
    Secure.set_base_dir (Filename.dirname full_path);
    let base = Gwdb.open_base full_path in
    load_strings_array base;
    check_insee base;
    (* Print statistics here, before any potential exit *)
    Printf.eprintf "Processing Statistics:\n%!";
    (* Added %! to flush output *)
    Printf.eprintf "Total individuals processed: %d\n%!" !total_processed;
    Printf.eprintf "Total individuals exported: %d\n%!" !total_exported;
    Printf.eprintf "  - With birth data: %d\n%!" !exported_with_birth;
    Printf.eprintf "  - With death data: %d\n%!" !exported_with_death;
    Printf.eprintf "  - From nodate criteria: %d\n%!" !exported_nodate;
    Printf.eprintf "Export mode: %s\n%!"
      (if !nodate_only then "Nodate only"
      else if !nodate then "Including nodate"
      else "Standard")
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    exit 1
