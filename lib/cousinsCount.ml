open Config
open Def
open Gwdb
open Util

(* call recursively this function for each sibling to count descendants *)
let rec count_descend_upto conf base cnt cnt_sp max_cnt ini_p ini_br lev children =
  match lev with
  | 0 -> (cnt, cnt_sp)
  | lev ->
			List.fold_left
				(fun (cnt, cnt_sp) (ip, ia_asex, rev_br) ->
          let p = pget conf base ip in
          let br = List.rev ((ip, get_sex p) :: rev_br) in
          let is_valid_rel = Cousins.br_inter_is_empty ini_br br in
          (if is_valid_rel && cnt < max_cnt
          then
						if lev > 1 && Cousins.has_desc_lev conf base lev p
						then
							begin
							 (* the function children_of returns *all* the children of ip *)
							 List.fold_left
								 (fun (cnt, cnt_sp) ifam ->
										let children =
											List.map
												(fun ip ->
													 (ip, ia_asex, (get_iper p, get_sex p) :: rev_br))
												(Cousins.children_of_fam base ifam)
										in
										count_descend_upto conf base cnt cnt_sp max_cnt ini_p ini_br (lev - 1) children
								 )
								 (cnt, cnt_sp) (Array.to_list (get_family p));
							end
						else
						  if lev = 1 then
								let nb_sp = Array.length (get_family p) in
								(cnt + 1, cnt_sp + nb_sp)
						  else (cnt, cnt_sp)
          else
						(cnt, cnt_sp)))
        (cnt, cnt_sp) children

let count_cousins_side_of conf base cnt cnt_sp max_cnt a ini_p ini_br lev1 lev2 =
  let sib = Cousins.siblings conf base (get_iper a) in
	if List.exists (Cousins.sibling_has_desc_lev conf base lev2) sib then
		begin
			let sib = List.map (fun (ip, ia_asex) -> ip, ia_asex, []) sib in
			count_descend_upto conf base cnt cnt_sp max_cnt ini_p ini_br lev2 sib
		end
	else (cnt, cnt_sp)

let count_cousins_lev conf base max_cnt p lev1 lev2 =
  let first_sosa =
    let rec loop sosa lev =
      if lev <= 1 then sosa else loop (Sosa.twice sosa) (lev - 1)
    in
    loop Sosa.one lev1
  in
  let last_sosa = Sosa.twice first_sosa in
  (* scan each sosa at level lev1  till first sosa at level lev1+1*)
	let rec loop sosa (cnt, cnt_sp) =
		if cnt < max_cnt && Sosa.gt last_sosa sosa then
			let (cnt, cnt_sp) =
				match Util.old_branch_of_sosa conf base (get_iper p) sosa with
					Some ((ia, _) :: _ as br) ->
					count_cousins_side_of conf base cnt cnt_sp max_cnt (pget conf base ia) p br lev1 lev2
					| _ -> (cnt, cnt_sp)
			in
			loop (Sosa.inc sosa 1) (cnt, cnt_sp)
		else (cnt, cnt_sp)
	in
	loop first_sosa (0, 0)
