open Def

module Gwdb = Gwdb
module DateDisplay = Geneweb.DateDisplay

let short_family_dates_text conf _base marr_sep fam =
  let marr_dates_aux =
    match Adef.od_of_cdate (Gwdb.get_marriage fam) with
    | Some d ->
      begin match d with
        | Dgreg (dmy, _) -> Some (DateDisplay.prec_year_text conf dmy)
        | _ -> Some ""
      end
    | _ -> Some ""
  in
  let sep_dates_aux =
    match List.find_opt (fun e ->
        e.efam_name = Efam_Divorce ||
        e.efam_name = Efam_Annulation ||
        e.efam_name = Efam_Separated)
        (Gwdb.get_fevents fam) with
    | Some e ->
      begin match Adef.od_of_cdate e.efam_date with
        | Some d ->
          begin match d with
            | Dgreg (dmy, _) -> Some (DateDisplay.prec_year_text conf dmy)
            | _ -> Some ""
          end
        | _ -> Some ""
      end
    | _ -> None
  in
  if marr_sep then
    match marr_dates_aux, sep_dates_aux with
    | Some m, Some s -> m ^ "–" ^ s
    | Some m, None -> m
    | _, _ -> ""
  else
     match sep_dates_aux with
    | Some m -> m
    | _ -> ""
