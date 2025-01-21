(* Copyright (c) 2019 Ludovic LEDIEU *)

open Gwdb
open Def

let my_uppercase2 s =
  let s = String.split_on_char '\'' s in
  String.concat
    (String.make 1 '\'')
    (List.map (fun e -> String.uppercase_ascii (Name.lower e)) s)

let my_uppercase s =
  let s = String.split_on_char '-' s in
  String.concat
    (String.make 1 '-')
    (List.map (fun e -> my_uppercase2 e) s)

let check_insee base =
  (* pour chaque personne *)
  Gwdb.Collection.iteri begin fun i p ->
    let b_date =
      match Date.od_of_cdate (get_birth p) with
        Some (Dgreg (d, _)) -> Some d
      | _ -> None
    in
    let d_date =
      match get_death p with
      | Death (_, cd) ->
        begin match Date.date_of_cdate cd with
          | Dgreg (d, _) -> Some d
          | _ -> None
        end
      | _ -> None
    in
    let sn = my_uppercase (p_surname base p) in
    let fn =
      let fn = p_first_name base p in
      begin match get_first_names_aliases p with
          [] -> fn
        | first :: _ ->
          let fna = sou base first in
          let re = Str.regexp_string (Str.quote fn) in
          begin try ignore (Str.search_forward re fna 0); fna
            with Not_found -> fn
          end
      end
    in
    let fn = my_uppercase fn in
    let s =
      match get_sex p with
        Male -> 1
      | Female -> 2
      | _ -> 0
    in
    let b_place = sou base (get_birth_place p) in
    let d_place = sou base (get_death_place p) in
    let key =
      sou base (get_first_name p) ^ "." ^ string_of_int (get_occ p) ^ " " ^ sou base (get_surname p)
    in
    let check =
      fn != "X" &&
      (b_place = "" || (Mutil.contains b_place ", France") ||
       d_place = "" || (Mutil.contains d_place ", France")) &&
      match d_date with
      | Some dd when dd.prec = Sure && dd.year < 1970 -> false
      | Some dd when dd.prec = Before && dd.year < 1970 -> false
      | _ -> true
    in
    match b_date with
      Some bd when bd.year > 1860 && check = true ->
      begin match d_date with
        | Some dd ->
          Printf.printf "%s|%s|%d|%02d|%02d|%04d|%s|%02d|%02d|%04d|%s|%s\n" sn fn s
            (if bd.prec = Sure then bd.day else 0)
            (if bd.prec = Sure then bd.month else 0)
            (if bd.prec = Sure || bd.prec = About then bd.year else 0)
            b_place
            (if dd.prec = Sure then dd.day else 0)
            (if dd.prec = Sure then dd.month else 0)
            (if dd.prec = Sure || dd.prec = About then dd.year else 0)
            d_place
            key
        | None ->
          Printf.printf "%s|%s|%d|%02d|%02d|%04d|%s|00|00|0000|%s|%s\n" sn fn s
            (if bd.prec = Sure then bd.day else 0)
            (if bd.prec = Sure then bd.month else 0)
            (if bd.prec = Sure || bd.prec = About then bd.year else 0)
            b_place
            d_place
            key
      end
    | _ ->
      if check then
        begin match d_date with
          | Some dd when dd.year > 1970 ->
            Printf.printf "%s|%s|%d|00|00|0000|%s|%02d|%02d|%04d|%s|%s\n" sn fn s
              b_place
              (if dd.prec = Sure then dd.day else 0)
              (if dd.prec = Sure then dd.month else 0)
              (if dd.prec = Sure || dd.prec = About then dd.year else 0)
              d_place
              key
          | _ -> ()
        end
  end (Gwdb.persons base)

(* main *)
let base_dir = ref "."
let base_name = ref ""

let speclist = [
  ("-bd", Arg.Set_string base_dir, "   Specify base directory")
]

let anon_fun s = 
  if !base_name = "" then base_name := s
  else raise (Arg.Bad "Cannot treat several databases")

(* Main execution *)
let () = 
  try
    Arg.parse speclist anon_fun "Usage: insee [-bd basedir] basename";
    let full_path = 
      if !base_name = "" then begin
        Arg.usage speclist "Usage: insee [-bd basedir] basename";
        exit 2
      end else begin
        if Filename.check_suffix !base_name ".gwb" 
        then Filename.concat !base_dir !base_name
        else Filename.concat !base_dir (!base_name ^ ".gwb")
      end
    in
    Secure.set_base_dir (Filename.dirname full_path);
    let base = Gwdb.open_base full_path in
    load_strings_array base;
    check_insee base
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    exit 1
