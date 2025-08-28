open Config
open RelationMatrix

(* Fonctions de formatage pour l’interface HTML *)
let format_ancestors_with_couples conf base
    (ancestors_with_ifam : (Driver.iper * Driver.ifam) list)
    (sols : (Driver.iper * int) list) : string =
  let items = group_ancestors_by_couples ancestors_with_ifam sols in
  let fmt = function
    | `Couple (ip1, c1, ip2, c2, _) ->
        let p1 = Driver.poi base ip1 in
        let p2 = Driver.poi base ip2 in
        Printf.sprintf "%s [%d] %s %s [%d]"
          (get_person_simple_name base p1)
          c1
          (Util.transl conf "and" :> string)
          (get_person_simple_name base p2)
          c2
    | `Unique (ip, cnt) ->
        let p = Driver.poi base ip in
        Printf.sprintf "%s [%d]" (get_person_simple_name base p) cnt
  in
  String.concat ", " (List.map fmt items)

let format_percentage_for_cell conf coefficient =
  let percentage = coefficient *. 100.0 in
  let abs_percentage = abs_float percentage in
  let formatted_number =
    if abs_percentage >= 1.0 then
      let rounded = Float.round (percentage *. 100.0) /. 100.0 in
      Util.string_of_decimal_num conf rounded
    else if abs_percentage >= 0.001 then
      let rounded = Float.round (percentage *. 1000.0) /. 1000.0 in
      Util.string_of_decimal_num conf rounded
    else ""
  in
  if formatted_number = "" then "" else formatted_number ^ "&nbsp;%"

let make_tooltip_text conf base p1 p2 (kd_opt : kinship_degree option)
    (total_sosa : Sosa.t) (rel_coeff : float) : string =
  let p1_name = get_person_simple_name base p1 in
  let p2_name = get_person_simple_name base p2 in
  let colon = Util.transl conf ":" in
  let total_links =
    if Sosa.eq total_sosa Sosa.zero then 0
    else
      match int_of_string (Sosa.to_string total_sosa) with
      | n when n >= 0 -> n
      | _ -> 0
  in
  let link_word =
    Util.transl_nth conf "relationship link/relationship links"
      (if total_links = 1 then 0 else 1)
  in
  let link_between =
    Printf.sprintf
      (Util.ftransl conf "%d %s links between %s and %s")
      total_links link_word p1_name p2_name
  in
  let header = Printf.sprintf "<b>%s</b><br><br>" link_between in
  let body =
    match kd_opt with
    | None -> ""
    | Some kd ->
        let shortest =
          Util.transl conf "shortest path" |> Utf8.capitalize_fst
        in
        let shortest_path = Printf.sprintf "<b>%s%s</b><br>" shortest colon in
        let deg_icon = "<i class='fa-solid fa-elevator fa-fw'></i>" in
        let asc_icon =
          "<i class='fa-solid fa-person-arrow-up-from-line fa-fw'></i>"
        in
        let desc_icon =
          "<i class='fa-solid fa-person-arrow-down-to-line fa-flip-horizontal \
           fa-fw'></i>"
        in
        let kin_label =
          Util.transl_nth conf "degree of kinship"
            (if kd.len1 + kd.len2 = 1 then 0 else 1)
        in
        let asc_idx = if kd.len1 = 1 then 0 else 2 in
        let desc_idx = if kd.len2 = 1 then 1 else 3 in
        let asc_label =
          Util.transl_nth conf "ascending/descending (degree)" asc_idx
        in
        let desc_label =
          Util.transl_nth conf "ascending/descending (degree)" desc_idx
        in
        let degres =
          Printf.sprintf
            "  %s <span class='rm-deg'>%d</span> %s<br>%s <span \
             class='rm-deg'>%d</span> %s<br>%s <span class='rm-deg'>%d</span> \
             %s<br>"
            asc_icon kd.len1 asc_label desc_icon kd.len2 desc_label deg_icon
            (kd.len1 + kd.len2) kin_label
        in
        let rel_pct_info =
          Printf.sprintf "<b>%s%s</b> %s<br><br>"
            (Util.transl_nth conf "relationship coefficient/percentage" 1
            |> Utf8.capitalize_fst)
            colon
            (Util.string_of_decimal_num conf (rel_coeff *. 100.0) ^ " %")
        in
        let anc_html =
          format_ancestors_with_couples conf base kd.ancestors kd.sols
        in
        let anc_label =
          Util.transl_nth conf "first common ancestor"
            (if List.length kd.ancestors = 1 then 0 else 1)
          |> Utf8.capitalize_fst
        in
        let common_anc =
          Printf.sprintf "<br><b>%s%s</b><br>%s" anc_label colon anc_html
        in
        rel_pct_info ^ shortest_path ^ degres ^ common_anc
  in
  String.map (function '"' -> '\'' | c -> c) (header ^ body)

let encoded_key name = (Mutil.encode (Name.lower name) :> string)

let make_relationship_link conf base p1 p2 =
  let p1_accessible =
    Util.accessible_by_key conf base p1
      (Driver.p_first_name base p1)
      (Driver.p_surname base p1)
  in
  let p2_accessible =
    Util.accessible_by_key conf base p2
      (Driver.p_first_name base p2)
      (Driver.p_surname base p2)
  in
  if p1_accessible && p2_accessible then
    let p1_fname = encoded_key (Driver.p_first_name base p1) in
    let p1_sname = encoded_key (Driver.p_surname base p1) in
    let oc1 = Driver.get_occ p1 in
    let p2_fname = encoded_key (Driver.p_first_name base p2) in
    let p2_sname = encoded_key (Driver.p_surname base p2) in
    let oc2 = Driver.get_occ p2 in
    let oc_param = if oc1 = 0 then "" else "&oc=" ^ string_of_int oc1 in
    let oc2_param = if oc2 = 0 then "" else "&oc2=" ^ string_of_int oc2 in
    Printf.sprintf "%sm=R&p=%s&n=%s%s&p2=%s&n2=%s%s"
      (Util.commd conf :> string)
      p1_fname p1_sname oc_param p2_fname p2_sname oc2_param
  else
    Printf.sprintf "%sm=R&i=%s&i2=%s"
      (Util.commd conf :> string)
      (Driver.Iper.to_string (Driver.get_iper p1))
      (Driver.Iper.to_string (Driver.get_iper p2))

(* Génération des éléments HTML de la matrice *)
let display_no_relationship_cell conf iper_i iper_j =
  Output.printf conf {|<td class="rm-cell" data-id="§%s_§%s">—</td>|}
    (Driver.Iper.to_string iper_i)
    (Driver.Iper.to_string iper_j)

let display_relationship_cell conf base person_i person_j iper_i iper_j notation
    kd total_sosa rel_coeff =
  let tooltip_text =
    make_tooltip_text conf base person_i person_j (Some kd) total_sosa rel_coeff
  in
  let links_count =
    Sosa.to_string_sep
      (Util.transl conf "(thousand separator)" :> string)
      total_sosa
  in
  let coeff_str =
    let pct = format_percentage_for_cell conf rel_coeff in
    if pct = "" then "" else "<small>" ^ pct ^ "</small>"
  in
  let cell_content =
    Printf.sprintf "<small>%s</small><br><b>%s</b><br>%s" notation links_count
      coeff_str
  in
  let relationship_url = make_relationship_link conf base person_i person_j in
  let aria_text =
    Printf.sprintf "%s %s %s %s"
      (Util.transl conf "relationship between" :> string)
      (get_person_simple_name base person_i)
      (Util.transl conf "and" :> string)
      (get_person_simple_name base person_j)
  in
  let cell_link =
    Util.make_link ~href:relationship_url ~content:cell_content
      ~aria_label:aria_text ()
  in
  Output.printf conf
    {|<td class="rm-cell" data-id="§%s_§%s" title="%s"
       data-toggle="tooltip" data-html="true" data-placement="top">
      %s
    </td>|}
    (Driver.Iper.to_string iper_i)
    (Driver.Iper.to_string iper_j)
    (String.map (function '"' -> '\'' | c -> c) tooltip_text)
    (cell_link :> string)

let print_matrix_cell conf base tstab persons person_ipers i j cell_storage n =
  let person_i = persons.(i) in
  let person_j = persons.(j) in
  let iper_i = person_ipers.(i) in
  let iper_j = person_ipers.(j) in
  if i = j then
    Output.printf conf {|<td class="rm-diag" data-id="§%s_§%s">—</td>|}
      (Driver.Iper.to_string iper_i)
      (Driver.Iper.to_string iper_j)
  else
    let cell_data_opt =
      if j > i then (
        let notation_opt, kd_opt, total_sosa, rel_coeff, all_ancestors =
          compute_relationship_cell conf base tstab person_i person_j
        in
        let cell_data =
          {
            notation = notation_opt;
            kd = kd_opt;
            total_sosa;
            rel_coeff;
            relationship_exists = notation_opt <> None;
            all_ancestors_with_ifam = all_ancestors;
          }
        in
        triangular_set cell_storage i j n (Some cell_data);
        Some cell_data)
      else
        match triangular_get cell_storage j i n with
        | None -> None
        | Some orig_cell ->
            if orig_cell.relationship_exists then
              Some (create_inverted_cell_data orig_cell)
            else None
    in
    match cell_data_opt with
    | None -> display_no_relationship_cell conf iper_i iper_j
    | Some cell_data -> (
        match (cell_data.notation, cell_data.kd) with
        | Some notation, Some kd ->
            display_relationship_cell conf base person_i person_j iper_i iper_j
              notation kd cell_data.total_sosa cell_data.rel_coeff
        | _ -> display_no_relationship_cell conf iper_i iper_j)

let print_matrix_headers conf persons person_ipers =
  let n = Array.length persons in
  Output.print_sstring conf {|<tr><th class="border-top-0"></th>|};
  for j = 0 to n - 1 do
    let iper_j = person_ipers.(j) in
    Output.printf conf
      {|<th class="text-center rm-thead" data-id="§%s_">
         <span class="badge badge-pill badge-primary">%d</span>
       </th>|}
      (Driver.Iper.to_string iper_j)
      (j + 1)
  done;
  Output.print_sstring conf {|</tr>|}

let print_matrix_row conf base tstab persons person_ipers i cell_storage n =
  let person = persons.(i) in
  let iper_i = person_ipers.(i) in
  Output.print_sstring conf "<tr>";
  Output.printf conf
    {|
    <th class="rm-head" data-id="§%s_">
      <div class="d-flex align-items-center">
        <span class="badge badge-pill badge-primary ml-2 mr-3">%d</span>
        <div class="flex-fill text-left">
          <div><a href="%s">%s</a></div>
          <div class="small text-muted ml-3">%s</div>
        </div>
      </div>
    </th>|}
    (Driver.Iper.to_string iper_i)
    (i + 1)
    (Util.acces conf base person :> string)
    (Util.referenced_person_text conf base person :> string)
    (DateDisplay.short_dates_text conf base person :> string);
  for j = 0 to n - 1 do
    print_matrix_cell conf base tstab persons person_ipers i j cell_storage n
  done;
  Output.print_sstring conf "</tr>"

(* Point d’entrée principal pour l’affichage *)
let print_matrix_table conf base persons =
  let tstab = Util.create_topological_sort conf base in
  let person_ipers = Array.map Driver.get_iper persons in
  let n = Array.length persons in
  let cell_storage = create_triangular_matrix n in
  Output.print_sstring conf
    {|<div class="d-flex justify-content-center">
      <table class="table table-sm w-auto" id="rm-table">
        <tbody>|};
  print_matrix_headers conf persons person_ipers;
  for i = 0 to n - 1 do
    print_matrix_row conf base tstab persons person_ipers i cell_storage n
  done;
  Output.print_sstring conf {|</tbody></table></div>|}

let print conf base =
  if Util.url_has_pnoc_params conf.env then
    let clean_url = Util.normalize_person_pool_url conf base "RM" None in
    Wserver.http_redirect_temporarily clean_url
  else
    let pl =
      let rec loop pl i =
        let k = string_of_int i in
        match Util.find_person_in_env conf base k with
        | Some p -> loop (p :: pl) (i + 1)
        | None -> List.rev pl
      in
      loop [] 1
    in
    let title _ =
      Util.transl_nth conf "relationship table" 2
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Util.print_loading_overlay conf ();
    let tp_link conf =
      let base_url = (Util.commd conf :> string) in
      let other_params =
        List.filter_map
          (fun (k, v) ->
            if k <> "m" then Some (k ^ "=" ^ Adef.as_string v) else None)
          conf.env
      in
      base_url ^ "m=TP&v=updRLM&" ^ String.concat "&" other_params
    in
    Output.printf conf
      {|<div class="d-flex justify-content-between align-items-center mb-3">
          <h2 class="h3">%s</h2>|}
      (Util.transl_nth conf "relationship table" 3 |> Utf8.capitalize_fst);
    Output.printf conf
      {|<a href="%s" class="btn btn-outline-primary">
          <i class="fas fa-edit mr-1"></i>%s</a></div>|}
      (tp_link conf)
      (Util.transl_nth conf "add/clear/show/edit the graph" 3
      |> Utf8.capitalize_fst);
    if List.length pl >= 2 then (
      let persons = Array.of_list pl in
      print_matrix_table conf base persons;
      Output.print_sstring conf "</div>";
      Output.print_sstring conf
        {|<script>
document.addEventListener('DOMContentLoaded', function() {
  $('.table [data-id]').hover(function() {
    var ids = $(this).data('id').split('_');
    $('[data-id*="' + ids[0] + '"]').toggleClass('rm-hl0');
    if (ids[1] && ids[1] !== ids[0]) {
      $('[data-id*="' + ids[1] + '"]').toggleClass('rm-hl1');
    }
  });
});
</script>|};
      Util.print_loading_overlay_js conf;
      Hutil.trailer conf)
