open Geneweb.Config
open Def
open Geneweb.TemplAst
open Gwdb
open Geneweb.Util
open Geneweb.Dag2html
open Geneweb.DescendDisplay

module DagDisplay = Geneweb.DagDisplay
module DateDisplay = Geneweb.DateDisplay
module Dag2html = Geneweb.Dag2html
module DescendDisplay = Geneweb.DescendDisplay
module Util = Geneweb.Util

(* new descendant tree algorithm *)
(* based on the work of Jean Vaucher (1942-2021) *)
(* - see issue #414 for comments and details *)

(* *****************************************************************
L'algo fait un parcours PRE-ORDRE de l'arbre en donnant une colonne 
initiale "logique" pour chaque personne, par exemple:  X-1 et X+1 
pour les 2 enfants d'un pere (P) dans la colonne X.
Ce "X" initial peut etre augmenté (déplacé à droite) pour 2 raisons:

	a) La case proposée est déjà occupée.  Un tableau global, 
	last_x, garde la position des derniers noeuds dans chaque rangée. 
	On modifie X = max( X, last_x[i]+2)  gardant au moins une colonne 
	vide entre chaque noeud.
	b) Pour la meme raison, il se peut que les enfants de la personne
	soient déplacés. ON termine en mettant X a mi-chemin entre les position
	finale du premier et dernier enfant.

***************************************************************** *)

(* one td is [nb_cols * align * content]  see dag2Html.ml *)

(* empty fill over n columns *)
let td_fill x1 xn = 
  [(xn - x1), CenterA, TDnothing]

(* hbar over xn - x1 columns. First and last are left/right and 50% *)
let td_hbar x1 xn =
  match xn - x1 with
  | 0 -> [1, CenterA,  TDtext (Gwdb.dummy_iper, "|")]
  | 1 -> [1, CenterA,  TDhr CenterA]
  | _ -> [1, LeftA, TDhr LeftA] @ [(xn - x1 - 1), CenterA, TDhr CenterA] @ [1, RightA, TDhr RightA]

(* regular cell, centered, with text as content (may contain |<br>) *)
let td_cell cols align text ip =
    match align with 
    | "center" -> [cols, CenterA, TDtext (ip, text)]
    | "right" -> [cols, RightA, TDtext (ip, text)]
    | "left" -> [cols, LeftA, TDtext (ip, text)]
    | _ -> [0, CenterA, TDtext (ip, text)]

(* tdal is   (int   *    list      ) list           *)
(*           (lastx      list of td) list of rows   *)

(* add elem to row ir. Update lastx *)
let tdal_add tdal ir elem nx =
  let tdal =
    let rec loop tdal new_tdal ir elem nx =
      match tdal with
      | [] -> new_tdal
      | (x, row) :: tdal ->
          if ir = 0 then
            let new_row = (nx, row @ elem) in
            new_row :: (loop tdal new_tdal (ir-1) elem nx)
          else
            (x, row) :: (loop tdal new_tdal (ir-1) elem nx)
    in loop tdal [] ir elem nx
  in
  tdal

let get_bd_td_prop conf =
  let bd =
    match Util.p_getint conf.env "bd" with
      Some x -> x
    | None -> 0
  in
  let td_prop =
    match Util.p_getenv conf.env "td" with
      Some x -> " " ^ x
    | _ ->
        match Util.p_getenv conf.env "color" with
          None | Some "" -> ""
        | Some x -> " class=\"" ^ x ^ "\""
  in
  (bd, td_prop)


let get_text conf base p =
  let (bd, td_prop) = get_bd_td_prop conf in
  let auth = authorized_age conf base p in
  let txt = person_title_text conf base p in
  let txt = reference conf base p txt in
  let txt =
    if auth then txt ^ DateDisplay.short_dates_text conf base p else txt
  in
  let txt = txt ^ DagDisplay.image_txt conf base p in
  let txt =
    if bd > 0 || td_prop <> "" then
      Printf.sprintf
        "<table style=\"border: %dpx solid\"><tr><td align=\"center\"%s>%s</td>\
         </tr></table>"
        bd td_prop txt
    else txt
  in
  txt

let lastx tdal ir =
  let (x, _) = List.nth tdal ir in x

let both_parents_in_only_anc p sp only_anc =
  let ip = get_iper p in
  let isp = get_iper sp in
  List.mem ip only_anc && List.mem isp only_anc

let get_spouse base iper ifam =
  let f = foi base ifam in
  if iper = get_father f then poi base (get_mother f)
  else poi base (get_father f)

(* the heart of Jean Vaucher algo                                    *)
(* to manage spouses=on, one must handle different ir numbering *)
(* insufficient to skip "display" of spouses WIP  default is on *)

(* for the "only" option , prune the family list according to   *)
(* relationship with target                                     *)

(*
        /                 |           (except for first row)
  ir   |               person
        \                 |           (except if no spouse )
  ir+1         ---- hbar over spouses ----  (| if only one)
        /                 |
  ir+2 |               spouse
        \                 |           (except if no child)
  ir+3          --- hbar over children ---- (| if only one)

  option spouses=off -> only 3 rows
  option only  -> ?

*)

let rec p_pos conf base p x0 v ir tdal only_anc spouses =
  let lx = lastx tdal ir in
  let x = if (lx+2) > x0 then (lx+2) else x0 in
  (*let x1 = x in
  let xn = x in *)
  let ifaml = List.rev (Array.to_list (get_family p)) in
  let descendants = ifaml <> [] in
  (* find right family there *)
  let ifaml =
    if only_anc <> [] then
      List.fold_left
        (fun acc ifam -> if List.mem ifam only_anc then ifam :: acc else acc)
      [] ifaml
    else ifaml
  in
  let (tdal, x, x1, xn) =
    if v > 1 && ifaml <> [] then
      let xn = if only_anc = [] then x - (List.length ifaml) - 1 else x in
      let (tdal, x1, xn) =
        let rec loop ifaml first x1 xn tdal =
          match ifaml with
          | [] -> tdal, x1, xn
          | ifam :: ifaml ->
              let (tdal, xn) = f_pos conf base ifam p (xn+2) v (ir+2) tdal only_anc spouses in
              loop ifaml false (if first then xn else x1) xn tdal
        in loop ifaml true 0 xn tdal
      in
      (tdal, (x1+xn)/2, x1, xn)
    else (tdal, x, x, x) (* ?? correct ?? *)
  in
  (* row 1: person *)
  let vv =
    match p_getenv conf.env "v" with
    | Some v -> "&v=" ^ v
    | None -> "&v=4"
  in
  let pp = Util.find_person_in_env conf base "" in
  let pp_index = match pp with
    | Some p -> "&i=" ^ (string_of_iper (get_iper p))
    | None -> ""
  in
  let pz = Util.find_person_in_env conf base "z" in
  let pz_index = match pz with
    | Some p -> "&iz=" ^ (string_of_iper (get_iper p))
    | None -> ""
  in  
  let txt = get_text conf base p in
  let only =
     Printf.sprintf "<a href=\"%sm=D&t=TV%s%s%s%s\" title=\"%s\">|</a>"
     (commd conf) vv pz_index pp_index ("&oi=" ^ (string_of_iper (get_iper p)))
     (Utf8.capitalize_fst (Util.transl conf "limit tree to ancestors and siblings"))
  in
  let text = if ir > 0 then only ^ "<br>" ^ txt else txt in
  (* ajouter un marqueur ici si enfants et on ne continue pas !!   *)
  let continue = only_anc = [] || ifaml <> [] in
  let br =
    match p_getenv conf.env "image" with
    | Some _ -> "<br>"
    | None -> ""
  in
  let text = if (not continue && descendants) then text ^ br ^ "+" else text in
  let lx = if lx > -1 then lx else -1 in
  let tdal = 
    tdal_add tdal ir ((td_fill lx (x - 1)) @ (td_cell 1 "center" text (get_iper p))) x
  in
  (* row 2: Hbar over spouses *)
  let tdal =
    if v > 1 && ifaml<>[] then
      let lx = lastx tdal (ir+1) in
      let lx = if lx > -1 then lx else -1 in
      if only_anc = [] then
        tdal_add tdal (ir+1) ((td_fill lx (x1 - 1)) @ (td_hbar x1 xn)) xn
      else
        tdal_add tdal (ir+1) ((td_fill lx (x - 1)) @ (td_cell 1 "center" "|" Gwdb.dummy_iper)) x
    else tdal
  in
  (tdal, x)

and f_pos conf base ifam p x0 v ir2 tdal only_anc spouses =
  let d_ir = if spouses then 1 else 0 in
  let sp = get_spouse base (get_iper p) ifam in
  let continue = (only_anc = []) || (List.mem ifam only_anc) in
  let lx = (lastx tdal ir2) + 2 in
  let x = if lx > x0 then lx else x0 in
  (*let x1 = x in
  let xn = x in *)
  let kids = Array.fold_left (fun l k -> (poi base k) :: l) [] (get_children (foi base ifam)) in
  let (tdal, x, x1, xn) =
    if kids <> [] && continue then
      let xn = x - (List.length kids) - 1 in
      let (tdal, x1, xn) =
        let rec loop kids first tdal x1 xn =
          match kids with
          | [] -> tdal, x1, xn
          | kid :: kids ->
              let (tdal, xn) = p_pos conf base kid (xn+2) (v-1) (ir2+d_ir+1) tdal only_anc spouses in
              loop kids false tdal (if first then xn else x1) xn
        in loop kids true tdal 0 xn
      in
      (tdal, (x1+xn)/2, x1, xn)
    else (tdal, x, x, x)
  in 
  (* row 3: spouses *)
  let txt = get_text conf base sp in
  let fam = foi base ifam in
  let marr_d = DateDisplay.short_marriage_date_text conf base fam p sp in
  let text = "& " ^ marr_d ^ " " ^ txt ^
    (if kids <> [] then "<br>|" else "")
  in 
  let lx = lastx tdal ir2 in
  let lx = if lx > -1 then lx else -1 in
  let tdal = 
    if spouses then
      tdal_add tdal ir2 ((td_fill lx (x - 1)) @ (td_cell 1 "center" text (get_iper sp))) x
    else tdal
  in
  (* rox 4: Hbar over kids *)
  if kids <> [] then
    let lx = lastx tdal (ir2+d_ir) in
    let lx = if lx > -1 then lx else -1 in
    let tdal =
      tdal_add tdal (ir2+d_ir) ((td_fill lx (x1 - 1)) @ (td_hbar x1 xn)) xn
    in
    (tdal, x)
  else (tdal, x)

let complete_rows tdal =
  let max_col =
    let rec loop tdal max_col =
      match tdal with
      | [] -> max_col
      | (x, row) :: tdal ->
          loop tdal (if x > max_col then x else max_col)
    in loop tdal (-2)
  in
  let tdal =
    let rec loop tdal new_td =
      match tdal with
      | [] -> new_td
      | (x, row) :: tdal ->
          if max_col - x > 0 then loop tdal ((max_col, row @ (td_fill x max_col)) :: new_td)
          else loop tdal ((x, row ) :: new_td)
    in loop tdal []
  in
  (List.rev tdal)

let init_tdal gv =
  let rec loop tdal v =
    match v with
    | 0 -> tdal (* 4 rows per generation  (3 if spouses=off) *)
    | _ -> loop ((0, []) :: (0, []) :: (0, []) :: (0, []) :: tdal) (v-1)
  in
  loop [] gv

(* remove x entry *)
let clean_rows tdal =
  let tdal = 
    let rec loop tdal new_tdal =
      match tdal with
      | [] -> new_tdal
      | (x, row) :: tdal ->
          let rec loop2 row new_row =
          match row with
          | [] -> loop tdal (new_row :: new_tdal)
          | td :: row -> loop2 row (td :: new_row)
          in loop2 row []
    in loop tdal []
  in
  (List.rev tdal)

(* manage vbars    *)
(* suppress lignes containing only vbars *)
(* in one colums, if there is nothing between two vbars, insert vbars *)
(* vbar = [1, CenterA,  TDtext "|"] filler is [(xn - x1), CenterA, TDnothing] *)
let manage_vbars tdal =
  let vbar_only_in_row row =
    List.fold_left (fun res (_, _, td) ->
      if td <> TDnothing && td <> (TDtext (Gwdb.dummy_iper, "|")) then false && res else true && res) true row
  in
  let tdal = List.fold_left (fun acc row ->
    if vbar_only_in_row row then acc else row :: acc) [] tdal
  in
  List.rev tdal

(* build a list of families (ifam) between iap and ip *)
let rec find_ancestors base iap ip list v =
  
  match get_parents (poi base ip) with
  | Some ifam ->
    let cpl = foi base ifam in
    let ifath = get_father cpl in
    let imoth = get_mother cpl in
    let list =
      if v > 1 && not (iap = ifath) then
        find_ancestors base iap ifath list (v-1)
      else list
    in
    let list =
      if v > 1 && not (iap = ifath) then
        find_ancestors base iap imoth list (v-1)
      else list
    in
    ifam :: list
  | None -> list

let make_vaucher_tree_hts conf base gv p =
  let spouses =
    match Util.p_getenv conf.env "spouses" with
    | Some _ -> false
    | _ -> true (* WIP, not working well so far *)
  in
  let (only_anc, op) =
    match Util.find_person_in_env_pref conf base "o" with
    | Some p -> (true, p)
    | None -> (false, (Gwdb.empty_person base Gwdb.dummy_iper))
  in
  let only_anc =
    if only_anc then find_ancestors base (get_iper p) (get_iper op) [] gv else []
  in
  let tdal = init_tdal gv in
  let (tdal, _) = p_pos conf base p 0 gv 0 tdal only_anc spouses in
  let tdal = complete_rows tdal in
  let tdal = clean_rows tdal in
  let tdal = manage_vbars tdal in
  let hts0 = List.fold_left (fun acc row -> (Array.of_list row) :: acc) [] tdal in
  let hts = Array.of_list (List.rev hts0) in
  hts

let print_vaucher_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    translate_eval
      (let s = person_text_no_html conf base p in
       transl_a_of_gr_eq_gen_lev conf (transl conf "descendants") s s)
  in
  let hts = make_vaucher_tree_hts conf base gv p in
  DagDisplay.print_slices_menu_or_dag_page conf page_title hts ""

let print conf base p =
  let templ =
    match p_getenv conf.env "t" with
      Some ("F" | "L" | "M") -> "deslist"
    | Some "D" -> "deslist_hr"
    | Some ("H" | "I" | "A") -> "destable"
    | Some "V" -> "destree"
    | Some _ -> ""
    | _ -> "desmenu"
  in
  if templ <> "" then !V7_interp.templ templ conf base p
  else
    match p_getenv conf.env "t", p_getint conf.env "v" with
      Some "B", Some v -> DescendDisplay.print_aboville conf base v p
    | Some "S", Some v -> DescendDisplay.display_descendants_level conf base v p
    | Some "K", Some v -> DescendDisplay.display_descendant_with_table conf base v p
    | Some "N", Some v -> DescendDisplay.display_descendants_with_numbers conf base v p
    | Some "G", Some v -> DescendDisplay.display_descendant_index conf base v p
    | Some "C", Some v -> DescendDisplay.display_spouse_index conf base v p
    | Some "T", Some v -> DescendDisplay.print_tree conf base v p
    | Some "TV", Some v -> print_vaucher_tree conf base v p
    | _ -> desmenu_print conf base p


