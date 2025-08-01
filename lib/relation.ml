(* Copyright (c) 1998-2007 INRIA *)

open Def
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Collection = Geneweb_db.Collection

let round_2_dec x = floor ((x *. 100.0) +. 0.5) /. 100.0

(* find shortest path :
 * parents, siblings, mates and children are at distance 1.
 *)
type famlink = Self | Parent | Sibling | HalfSibling | Mate | Child

type 'a dag_ind = {
  di_val : 'a;
  mutable di_famc : 'a dag_fam;
  mutable di_fams : 'a dag_fam;
}

and 'a dag_fam = {
  mutable df_pare : 'a dag_ind list;
  df_chil : 'a dag_ind list;
}

type person_id = Driver.iper
type family_id = Driver.ifam
type path_count = int
type degree = int

(** Structure compacte pour les chemins de parent�
    - p : person identifier (iper)
    - f : family identifiers list (ifams)
    - c : count of paths through this ancestor
    - l1 : ascending degrees from person 1
    - l2 : ascendante degrees from/descending degrees to person 2
    - anc : ancestors list for this path *)

type anc_f = { p : Driver.iper; f : Driver.ifam list; c : int }
type path_f = { l1 : int; l2 : int; anc : anc_f list }

let dag_ind_list_of_path path =
  let indl, _ =
    let merge l1 l2 = if l1 == l2 then l1 else l1 @ l2 in
    List.fold_left
      (fun (indl, prev_ind) (ip, fl) ->
        let ind, indl =
          try (List.find (fun di -> di.di_val = Some ip) indl, indl)
          with Not_found ->
            let rec ind = { di_val = Some ip; di_famc = famc; di_fams = fams }
            and famc = { df_pare = []; df_chil = [ ind ] }
            and fams = { df_pare = [ ind ]; df_chil = [] } in
            (ind, ind :: indl)
        in
        let fam =
          match prev_ind with
          | None -> { df_pare = []; df_chil = [] }
          | Some p_ind -> (
              match fl with
              | Parent ->
                  {
                    df_pare = merge p_ind.di_famc.df_pare ind.di_fams.df_pare;
                    df_chil = merge p_ind.di_famc.df_chil ind.di_fams.df_chil;
                  }
              | Child ->
                  {
                    df_pare = merge p_ind.di_fams.df_pare ind.di_famc.df_pare;
                    df_chil = merge p_ind.di_fams.df_chil ind.di_famc.df_chil;
                  }
              | Sibling | HalfSibling ->
                  {
                    df_pare = merge p_ind.di_famc.df_pare ind.di_famc.df_pare;
                    df_chil = merge p_ind.di_famc.df_chil ind.di_famc.df_chil;
                  }
              | Mate ->
                  {
                    df_pare = merge p_ind.di_fams.df_pare ind.di_fams.df_pare;
                    df_chil = merge p_ind.di_fams.df_chil ind.di_fams.df_chil;
                  }
              | Self -> { df_pare = []; df_chil = [] })
        in
        List.iter (fun ind -> ind.di_famc <- fam) fam.df_chil;
        List.iter (fun ind -> ind.di_fams <- fam) fam.df_pare;
        (indl, Some ind))
      ([], None) (List.rev path)
  in
  indl

let add_missing_parents_of_siblings conf base indl =
  List.fold_right
    (fun ind indl ->
      let indl =
        match ind.di_famc with
        | { df_pare = []; df_chil = [ _ ] } -> indl
        | { df_pare = []; df_chil = children } ->
            let ipl =
              List.fold_right
                (fun ind ipl ->
                  match ind.di_val with
                  | Some ip ->
                      let ip =
                        match Driver.get_parents (pget conf base ip) with
                        | Some ifam -> Driver.get_father (Driver.foi base ifam)
                        | None -> assert false
                      in
                      if List.mem ip ipl then ipl else ip :: ipl
                  | _ -> assert false)
                children []
            in
            let fams = { df_pare = []; df_chil = children } in
            let indl1 =
              List.fold_left
                (fun indl ip ->
                  let rec indp =
                    { di_val = Some ip; di_famc = famc; di_fams = fams }
                  and famc = { df_pare = []; df_chil = [ indp ] } in
                  fams.df_pare <- indp :: fams.df_pare;
                  indp :: indl)
                [] ipl
            in
            List.iter (fun ind -> ind.di_famc <- fams) children;
            indl1 @ indl
        | _ -> indl
      in
      ind :: indl)
    indl []

let dag_fam_list_of_ind_list indl =
  List.fold_left
    (fun faml ind ->
      let faml =
        if List.memq ind.di_famc faml then faml else ind.di_famc :: faml
      in
      if List.memq ind.di_fams faml then faml else ind.di_fams :: faml)
    [] indl

let add_phony_children indl faml =
  List.fold_right
    (fun fam indl ->
      match fam with
      | { df_pare = [ _ ]; df_chil = [] } -> indl
      | { df_pare = pare; df_chil = [] } ->
          let rec ind = { di_val = None; di_famc = famc; di_fams = fams }
          and famc = { df_pare = pare; df_chil = [ ind ] }
          and fams = { df_pare = [ ind ]; df_chil = [] } in
          List.iter (fun ind -> ind.di_fams <- famc) pare;
          ind :: indl
      | _ -> indl)
    faml indl

let add_common_parent base ip1 ip2 set =
  let a1 = Driver.poi base ip1 in
  let a2 = Driver.poi base ip2 in
  match (Driver.get_parents a1, Driver.get_parents a2) with
  | Some ifam1, Some ifam2 ->
      let cpl1 = Driver.foi base ifam1 in
      let cpl2 = Driver.foi base ifam2 in
      if Driver.get_father cpl1 = Driver.get_father cpl2 then
        Dag.Pset.add (Driver.get_father cpl1) set
      else if Driver.get_mother cpl1 = Driver.get_mother cpl2 then
        Dag.Pset.add (Driver.get_mother cpl1) set
      else set
  | _ -> set

let ind_set_of_relation_path base path =
  let set, _ =
    List.fold_left
      (fun (set, prev_ip) (ip, fl) ->
        let set =
          match fl with
          | Parent | Child | Self | Mate -> set
          | Sibling | HalfSibling -> (
              match prev_ip with
              | Some prev_ip -> add_common_parent base prev_ip ip set
              | None -> set)
        in
        (Dag.Pset.add ip set, Some ip))
      (Dag.Pset.empty, None) (List.rev path)
  in
  set

type node = NotVisited | Visited of (bool * Driver.iper * famlink)

let excl_faml conf base =
  let rec loop list i =
    match p_getenv conf.Config.env ("ef" ^ string_of_int i) with
    | Some k -> loop (Driver.Ifam.of_string k :: list) (i + 1)
    | None -> (
        match find_person_in_env conf base ("ef" ^ string_of_int i) with
        | Some p ->
            let n =
              p_getint conf.env ("fef" ^ string_of_int i)
              |> Option.value ~default:0
            in
            let list =
              if n < Array.length (Driver.get_family p) then
                (Driver.get_family p).(n) :: list
              else list
            in
            loop list (i + 1)
        | None -> list)
  in
  loop [] 0

let get_shortest_path_relation conf base ip1 ip2 (excl_faml : Driver.ifam list)
    =
  let mark_per = Driver.iper_marker (Driver.ipers base) NotVisited in
  let mark_fam = Driver.ifam_marker (Driver.ifams base) false in
  List.iter (fun i -> Collection.Marker.set mark_fam i true) excl_faml;
  let parse_fam ifam =
    if Collection.Marker.get mark_fam ifam then []
    else
      let fam = Driver.foi base ifam in
      Collection.Marker.set mark_fam ifam true;
      let result =
        Array.fold_right
          (fun fam children ->
            if ifam = fam then children
            else if Collection.Marker.get mark_fam fam then children
            else
              Array.fold_right
                (fun child children -> (child, HalfSibling, fam) :: children)
                (Driver.get_children (Driver.foi base fam))
                children)
          (Driver.get_family (pget conf base (Driver.get_mother fam)))
          []
      in
      let result =
        Array.fold_right
          (fun fam children ->
            if ifam = fam then children
            else if Collection.Marker.get mark_fam fam then children
            else
              Array.fold_right
                (fun child children -> (child, HalfSibling, fam) :: children)
                (Driver.get_children (Driver.foi base fam))
                children)
          (Driver.get_family (pget conf base (Driver.get_father fam)))
          result
      in
      let result =
        Array.fold_right
          (fun child children -> (child, Sibling, ifam) :: children)
          (Driver.get_children (Driver.foi base ifam))
          result
      in
      (Driver.get_father fam, Parent, ifam)
      :: (Driver.get_mother fam, Parent, ifam)
      :: result
  in
  let neighbours iper =
    Array.fold_right
      (fun ifam nb ->
        if Collection.Marker.get mark_fam ifam then nb
        else
          let fam = Driver.foi base ifam in
          Collection.Marker.set mark_fam ifam true;
          Array.fold_right
            (fun child children -> (child, Child, ifam) :: children)
            (Driver.get_children fam)
            [
              (Driver.get_father fam, Mate, ifam);
              (Driver.get_mother fam, Mate, ifam);
            ]
          @ nb)
      (Driver.get_family (pget conf base iper))
      (Option.fold ~none:[] ~some:parse_fam
         (Driver.get_parents (pget conf base iper)))
  in
  let rec make_path path vertex =
    match path with
    | (_, Self) :: _ -> path
    | _ -> (
        match Collection.Marker.get mark_per vertex with
        | NotVisited -> assert false
        | Visited (_, v, f) -> make_path ((vertex, f) :: path) v)
  in
  let merge_path p1 p2 =
    let swap_order el =
      match el with
      | iper, Parent -> (iper, Child)
      | iper, Child -> (iper, Parent)
      | _ -> el
    in
    List.rev_append
      (List.rev_map2
         (fun (ip1, _) (_, fl2) -> swap_order (ip1, fl2))
         (List.rev (List.tl (List.rev p1)))
         (List.tl p1))
      (List.rev p2)
  in
  let one_step_further source queue =
    let rec loop1 newvertexlist = function
      | vertex :: vertexlist ->
          let rec loop2 result = function
            | (iper, fl, ifam) :: neighbourslist -> (
                match Collection.Marker.get mark_per iper with
                | NotVisited ->
                    Collection.Marker.set mark_per iper
                      (Visited (source, vertex, fl));
                    loop2 (iper :: result) neighbourslist
                | Visited (s, v, f) ->
                    if s = source then loop2 result neighbourslist
                    else
                      let p1 = make_path [ (iper, fl) ] vertex in
                      let p2 = make_path [ (iper, f) ] v in
                      let path =
                        if source then merge_path p2 p1 else merge_path p1 p2
                      in
                      Left (path, ifam))
            | [] -> loop1 result vertexlist
          in
          loop2 newvertexlist (neighbours vertex)
      | [] -> Right newvertexlist
    in
    loop1 [] queue
  in
  let rec width_search queue1 visited1 queue2 visited2 =
    if queue1 = [] || queue2 = [] then None
    else if visited1 > visited2 then
      let visited2 = visited2 + List.length queue2 in
      match one_step_further false queue2 with
      | Left (path, ifam) -> Some (path, ifam)
      | Right queue2 -> width_search queue1 visited1 queue2 visited2
    else
      let visited1 = visited1 + List.length queue1 in
      match one_step_further true queue1 with
      | Left (path, ifam) -> Some (path, ifam)
      | Right queue1 -> width_search queue1 visited1 queue2 visited2
  in
  Collection.Marker.set mark_per ip1 @@ Visited (true, ip1, Self);
  Collection.Marker.set mark_per ip2 @@ Visited (false, ip2, Self);
  width_search [ ip1 ] 0 [ ip2 ] 0

(** [simplify_path conf base path] Removes unnecessary people from the path
    (e.g. half sibling when only parents are useful)

    [ (HalfSibling|Sibling|Child) as x -> Child -> HalfSibling ] becomes
    [ x -> Mate -> Child ]

    [ HalfSibling -> Parent ] becomes [ Parent -> Mate -> Mate -> Parent ] *)
let simplify_path base path =
  let get get i =
    let p = Driver.poi base i in
    match Driver.get_parents p with
    | None -> assert false
    | Some parents -> get (Driver.foi base parents)
  in
  let aux get_field ht i =
    match Hashtbl.find_opt ht i with
    | Some r -> r
    | None ->
        let r = get get_field i in
        Hashtbl.add ht i r;
        r
  in
  let mother = aux Driver.get_mother (Hashtbl.create 0) in
  let father = aux Driver.get_father (Hashtbl.create 0) in
  let rec simplify = function
    | [] -> []
    | ((i1, (HalfSibling | Sibling | Child)) as x)
      :: (i2, Child)
      :: (_, HalfSibling)
      :: tl ->
        x
        :: (if father i1 = father i2 then (mother i2, Mate)
            else (father i2, Mate))
        :: simplify tl
    | ((i1, _r1) as x1) :: (i2, HalfSibling) :: (i3, Parent) :: tl ->
        if father i1 = father i2 then
          x1
          :: (father i2, Parent)
          :: (mother i2, Mate)
          :: (if mother i2 = i3 then simplify tl else (i3, Mate) :: simplify tl)
        else
          x1
          :: (mother i2, Parent)
          :: (father i2, Mate)
          :: (if father i2 = i3 then simplify tl else (i3, Mate) :: simplify tl)
    | x :: tl -> x :: simplify tl
  in
  let rec loop path =
    let path' = simplify path in
    if path = path' then path else loop path'
  in
  loop path

let nb_fields s =
  let rec loop cnt i =
    if i = String.length s then cnt
    else if s.[i] = '/' then loop (cnt + 1) (i + 1)
    else loop cnt (i + 1)
  in
  loop 1 0

let rec belongs_to_branch ip dist = function
  | (n, _, ipl) :: lens ->
      if n = dist && List.mem ip ipl then true
      else belongs_to_branch ip dist lens
  | [] -> false

(* FIXME: remove Array.to_list and List.hd !!*)
let get_piece_of_branch conf base (((reltab, list), x), proj) (len1, len2) =
  let anc, _ = List.hd list in
  let rec loop ip dist =
    if dist <= len1 then []
    else
      let lens = proj @@ Collection.Marker.get reltab ip in
      let rec loop1 = function
        | ifam :: ifaml ->
            let rec loop2 = function
              | ipc :: ipl ->
                  if belongs_to_branch ipc dist lens then
                    let dist = dist - 1 in
                    if dist <= len2 then ipc :: loop ipc dist else loop ipc dist
                  else loop2 ipl
              | [] -> loop1 ifaml
            in
            loop2 (Array.to_list (Driver.get_children (Driver.foi base ifam)))
        | [] -> []
      in
      loop1 (Array.to_list (Driver.get_family (pget conf base ip)))
  in
  loop (Driver.get_iper anc) x

let compute_simple_relationship conf base tstab ip1 ip2 =
  let tab = Consang.make_relationship_info base tstab in
  let relationship, ancestors_with_ifam =
    Consang.relationship_and_links base tab true ip1 ip2
  in
  if ancestors_with_ifam = [] then None
  else
    let ifam_table = Hashtbl.create 17 in
    List.iter
      (fun (iper, ifam) ->
        let existing =
          try Hashtbl.find ifam_table iper with Not_found -> []
        in
        if not (List.mem ifam existing) then
          Hashtbl.replace ifam_table iper (ifam :: existing))
      ancestors_with_ifam;
    let ancestors = List.map fst ancestors_with_ifam in
    let total =
      try
        List.fold_left
          (fun n i ->
            let u = Collection.Marker.get tab.Consang.reltab i in
            List.fold_left
              (fun n (_, n1, _) ->
                let n1 = if n1 < 0 then raise Exit else Sosa.of_int n1 in
                List.fold_left
                  (fun n (_, n2, _) -> Sosa.add n (Sosa.mul n1 n2))
                  n u.Consang.lens1)
              n u.Consang.lens2)
          Sosa.zero ancestors
      with Exit -> Sosa.zero
    in
    let rl =
      List.fold_left
        (fun rl i ->
          let u = Collection.Marker.get tab.Consang.reltab i in
          let p = pget conf base i in
          List.fold_left
            (fun rl (len1, n1, _) ->
              List.fold_left
                (fun rl (len2, n2, _) ->
                  let n = n1 * n2 in
                  let n = if n1 < 0 || n2 < 0 || n < 0 then -1 else n in
                  (len1, len2, (p, n)) :: rl)
                rl u.Consang.lens2)
            rl u.Consang.lens1)
        [] ancestors
    in
    let rl =
      List.sort
        (fun (len11, len12, _) (len21, len22, _) ->
          if len11 + len12 > len21 + len22 then -1
          else if len11 + len12 < len21 + len22 then 1
          else compare len21 len11)
        rl
    in
    let rl =
      List.fold_left
        (fun l (len1, len2, sol) ->
          match l with
          | (l1, l2, sols) :: l when len1 = l1 && len2 = l2 ->
              (l1, l2, sol :: sols) :: l
          | _ -> (len1, len2, [ sol ]) :: l)
        [] rl
    in
    let paths_with_ifams =
      List.map
        (fun (l1, l2, person_count_list) ->
          let count_table = Hashtbl.create 17 in
          List.iter
            (fun (person, cnt) ->
              let iper = Driver.get_iper person in
              let current =
                try Hashtbl.find count_table iper with Not_found -> 0
              in
              Hashtbl.replace count_table iper (current + cnt))
            person_count_list;
          let anc_list =
            Hashtbl.fold
              (fun iper count acc ->
                let ifams =
                  try Hashtbl.find ifam_table iper with Not_found -> []
                in
                { p = iper; f = ifams; c = count } :: acc)
              count_table []
          in
          { l1; l2; anc = anc_list })
        rl
    in
    Some (paths_with_ifams, total, relationship, ancestors_with_ifam)

let compute_simple_relationship_compat conf base tstab ip1 ip2 =
  match compute_simple_relationship conf base tstab ip1 ip2 with
  | None -> None
  | Some (paths, total, relationship, _) ->
      let rl =
        List.map
          (fun path ->
            let person_list =
              List.map (fun anc -> (Driver.poi base anc.p, anc.c)) path.anc
            in
            (path.l1, path.l2, person_list))
          paths
      in
      let reltab = (Consang.make_relationship_info base tstab).Consang.reltab in
      Some (rl, total, relationship, reltab)

let known_spouses_list conf base p excl_p =
  let u = p in
  Array.fold_left
    (fun spl ifam ->
      let sp =
        pget conf base (Gutil.spouse (Driver.get_iper p) (Driver.foi base ifam))
      in
      if
        Driver.sou base (Driver.get_first_name sp) <> "?"
        && Driver.sou base (Driver.get_surname sp) <> "?"
        && Driver.get_iper sp <> Driver.get_iper excl_p
      then sp :: spl
      else spl)
    [] (Driver.get_family u)

let merge_relations rl1 rl2 =
  List.merge
    (fun (po11, po12, (l11, l12, _), _) (po21, po22, (l21, l22, _), _) ->
      if l11 + l12 < l21 + l22 then -1
      else if l11 + l12 > l21 + l22 then 1
      else if l11 < l21 then -1
      else if l11 > l21 then 1
      else if po11 = None && po12 = None then -1
      else if po21 = None && po22 = None then 1
      else if po11 = None || po21 = None then -1
      else if po21 = None || po22 = None then 1
      else -1)
    rl1 rl2

let combine_relationship conf base tstab pl1 pl2 f_sp1 f_sp2 sl =
  List.fold_right
    (fun p1 sl ->
      List.fold_right
        (fun p2 sl ->
          let sol =
            compute_simple_relationship_compat conf base tstab
              (Driver.get_iper p1) (Driver.get_iper p2)
          in
          match sol with
          | Some (rl, total, _, reltab) ->
              let s = List.map (fun r -> (f_sp1 p1, f_sp2 p2, r)) rl in
              (s, total, reltab) :: sl
          | None -> sl)
        pl2 sl)
    pl1 sl

let sp p = Some p
let no_sp _ = None

let compute_relationship conf base by_marr p1 p2 =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  if ip1 = ip2 then None
  else
    let tstab = Util.create_topological_sort conf base in
    let sol = compute_simple_relationship_compat conf base tstab ip1 ip2 in
    let sol_by_marr =
      if by_marr then
        let spl1 = known_spouses_list conf base p1 p2 in
        let spl2 = known_spouses_list conf base p2 p1 in
        let sl = [] in
        let sl =
          match sol with
          | Some ((_, 0, _) :: _, _, _, _) -> sl
          | _ -> combine_relationship conf base tstab [ p1 ] spl2 no_sp sp sl
        in
        let sl =
          match sol with
          | Some ((0, _, _) :: _, _, _, _) -> sl
          | _ -> combine_relationship conf base tstab spl1 [ p2 ] sp no_sp sl
        in
        match (sol, sl) with
        | Some ((x1, x2, _) :: _, _, _, _), _ when x1 = 0 || x2 = 0 -> sl
        | _, ((_, _, (x1, x2, _)) :: _, _, _) :: _ when x1 = 0 || x2 = 0 -> sl
        | _ -> combine_relationship conf base tstab spl1 spl2 sp sp sl
      else []
    in
    let all_sol, rel =
      match sol with
      | Some (rl, total, rel, reltab) ->
          let s = List.map (fun r -> (None, None, r)) rl in
          ((s, total, reltab) :: sol_by_marr, rel)
      | None -> (sol_by_marr, 0.0)
    in
    let sl, total =
      List.fold_right
        (fun (rl1, total1, reltab) (rl, total) ->
          let rl1 =
            List.map (fun (po1, po2, list) -> (po1, po2, list, reltab)) rl1
          in
          (merge_relations rl1 rl, Sosa.add total1 total))
        all_sol ([], Sosa.zero)
    in
    if sl = [] then None else Some (sl, total, rel)
