(* Audit of the folding of the keys (Unidecode, Name.lower) on the first names
   and surnames of a base, read from the index of its dictionaries.

   <base>.inventory.tsv: one line per non-ASCII code point found, with its
   folding by the current table (status mapped, removed or unmapped; invalid
   for invalid UTF-8), the candidate folding when one is simulated (none, same
   or changed) and the numbers of strings and persons using it.

   With -table or -fallback, the keys are simulated with the candidate folding:
   - <base>.keys.tsv: the persons whose key changes, with their old and new
     keys, whether the bucket of names.inx changes, whether their old URL
     (p=, n=) still finds them, and the check of the new key: ok, collision,
     unchecked (not looked up in the base, see [fixed_point]) or nokey (empty
     first name or surname);
   - <base>.collisions.tsv: the new keys held by several persons, the persons
     whose key does not change marked "no";
   - <base>.files.tsv: the files named after the old keys (portraits, blasons,
     carrousels, history) and their new names. *)

open Geneweb
module Driver = Geneweb_db.Driver
module Dirs = Geneweb_dirs

let ( // ) = Filename.concat

type kind = Fn | Sn
type status = Mapped of string | Removed | Unmapped

type stat = {
  mutable fn_strings : int;
  mutable fn_persons : int;
  mutable sn_strings : int;
  mutable sn_persons : int;
  mutable examples : (int * string) list;
}

type scan = {
  entries : int;
  non_ascii : int;
  changed : Driver.istr list;
  mismatches : string list;
}

type entry = {
  iper : Driver.iper;
  first_name : string;
  surname : string;
  occ : int;
  new_fn : string;
  new_sn : string;
}

type options = {
  bases_dir : string;
  bname : string;
  out : string;
  table : string option;
  fallback : bool;
  fast : bool;
}

let utf_8 u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

let hex s =
  String.concat " "
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02X" (Char.code s.[i])))

let tsv s = String.map (function '\t' | '\n' | '\r' -> ' ' | c -> c) s
let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

let status u =
  let s = utf_8 u in
  Unidecode.decode
    (fun _ t -> if t = "" then Removed else Mapped t)
    (fun _ c -> Mapped (String.make 1 c))
    (fun _ -> Unmapped)
    s 0 (String.length s)

let output_of u =
  match status u with
  | Mapped t -> Some t
  | Removed -> Some ""
  | Unmapped -> None

let nfd u =
  let n = Uunf.create `NFD in
  let rec add acc v =
    match Uunf.add n v with
    | `Uchar c -> add (c :: acc) `Await
    | `Await | `End -> acc
  in
  List.rev (add (add [] (`Uchar u)) `End)

let lower_utf_8 u =
  match Uucp.Case.Map.to_lower u with
  | `Self -> utf_8 u
  | `Uchars l -> String.concat "" (List.map utf_8 l)

let case_output mapped u =
  let single m =
    match m with `Uchars [ c ] -> Some c | `Self | `Uchars _ -> None
  in
  List.find_map mapped
    (List.filter_map single
       [ Uucp.Case.Map.to_lower u; Uucp.Case.Map.to_upper u ])

(* The generic folding of a character the table does not map: nothing for a
   combining mark; the folding of its base letter for a letter decomposed into
   one letter and combining marks (Ș, ǰ, ї, ἀ); the folding of its other case
   (ſ, ʉ, Ь); its lowercase otherwise, so that the key does not depend on the
   case. A base letter is folded by the table or by its other case only:
   syllables (Hangul) and letters whose marks change the sound (kana) keep
   their own lowercase. *)
let fallback mapped u =
  if Uunf.ccc u > 0 then ""
  else
    let known c =
      match mapped c with Some _ as t -> t | None -> case_output mapped c
    in
    let decomposed =
      match List.filter (fun c -> Uunf.ccc c = 0) (nfd u) with
      | [ c ] when not (Uchar.equal c u) -> known c
      | [] | _ :: _ -> None
    in
    match decomposed with
    | Some t -> t
    | None -> (
        match case_output mapped u with Some t -> t | None -> lower_utf_8 u)

let mapped table u =
  match Hashtbl.find_opt table (Uchar.to_int u) with
  | Some _ as t -> t
  | None -> output_of u

let make_override table with_fallback =
  let memo = Hashtbl.create 256 in
  fun u ->
    let cp = Uchar.to_int u in
    match Hashtbl.find_opt memo cp with
    | Some r -> r
    | None ->
        let r =
          match Hashtbl.find_opt table cp with
          | Some _ as t -> t
          | None ->
              if with_fallback && Option.is_none (output_of u) then
                Some (fallback (mapped table) u)
              else None
        in
        Hashtbl.replace memo cp r;
        r

(* A valid sequence has the length Unidecode.decode gives it: the characters
   without candidate folding are folded by Name.unaccent_utf_8 itself. *)
let fold override s i =
  let d = String.get_utf_8_uchar s i in
  let t =
    if Uchar.utf_decode_is_valid d then override (Uchar.utf_decode_uchar d)
    else None
  in
  match t with
  | Some t -> (String.lowercase_ascii t, i + Uchar.utf_decode_length d)
  | None -> Name.unaccent_utf_8 true s i

let simulated_lower override s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec copy special i =
    if i < len then
      if Char.code s.[i] < 0x80 then
        match s.[i] with
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.') as c ->
            if special then Buffer.add_char b ' ';
            Buffer.add_char b (Char.lowercase_ascii c);
            copy false (i + 1)
        | _ -> copy (Buffer.length b <> 0) (i + 1)
      else (
        if special then Buffer.add_char b ' ';
        let t, j = fold override s i in
        Buffer.add_string b t;
        copy false j)
  in
  copy false 0;
  Buffer.contents b

let codepoints s =
  let len = String.length s in
  let rec loop acc i =
    if i >= len then acc
    else if Char.code s.[i] < 0x80 then loop acc (i + 1)
    else
      let d = String.get_utf_8_uchar s i in
      let cp =
        if Uchar.utf_decode_is_valid d then
          Uchar.to_int (Uchar.utf_decode_uchar d)
        else -1
      in
      loop
        (if List.mem cp acc then acc else cp :: acc)
        (i + Uchar.utf_decode_length d)
  in
  loop [] 0

let iter_entries spi f =
  match Driver.spi_first spi "" with
  | exception Not_found -> ()
  | first ->
      let rec loop istr =
        f istr;
        match Driver.spi_next spi istr with
        | exception Not_found -> ()
        | next -> loop next
      in
      loop first

let new_stat () =
  {
    fn_strings = 0;
    fn_persons = 0;
    sn_strings = 0;
    sn_persons = 0;
    examples = [];
  }

let add_example n s examples =
  List.filteri
    (fun i _ -> i < 3)
    (List.merge (fun (a, _) (b, _) -> Int.compare b a) [ (n, s) ] examples)

let record stats kind n s cp =
  let st =
    match Hashtbl.find_opt stats cp with
    | Some st -> st
    | None ->
        let st = new_stat () in
        Hashtbl.replace stats cp st;
        st
  in
  (match kind with
  | Fn ->
      st.fn_strings <- st.fn_strings + 1;
      st.fn_persons <- st.fn_persons + n
  | Sn ->
      st.sn_strings <- st.sn_strings + 1;
      st.sn_persons <- st.sn_persons + n);
  let prefix = match kind with Fn -> "fn:" | Sn -> "sn:" in
  st.examples <- add_example n (prefix ^ s) st.examples

let scan_dict base stats override kind spi =
  let entries = ref 0 in
  let non_ascii = ref 0 in
  let changed = ref [] in
  let mismatches = ref [] in
  iter_entries spi (fun istr ->
      incr entries;
      let s = Driver.sou base istr in
      if not (is_ascii s) then (
        incr non_ascii;
        let cps = codepoints s in
        List.iter (record stats kind (Driver.spi_count spi istr) s) cps;
        match override with
        | None -> ()
        | Some override ->
            let old_key = Name.lower s in
            if simulated_lower (fun _ -> None) s <> old_key then
              mismatches := s :: !mismatches;
            if
              List.exists
                (fun cp ->
                  cp >= 0 && Option.is_some (override (Uchar.of_int cp)))
                cps
              && simulated_lower override s <> old_key
            then changed := istr :: !changed));
  {
    entries = !entries;
    non_ascii = !non_ascii;
    changed = !changed;
    mismatches = !mismatches;
  }

let read_table fname =
  let table = Hashtbl.create 64 in
  let error n msg = failwith (Printf.sprintf "%s, line %d: %s" fname n msg) in
  let is_hex c =
    match c with '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true | _ -> false
  in
  let codepoint n s =
    let len = String.length s in
    if
      len >= 6 && len <= 8
      && String.sub s 0 2 = "U+"
      && String.for_all is_hex (String.sub s 2 (len - 2))
    then
      let cp = int_of_string ("0x" ^ String.sub s 2 (len - 2)) in
      if cp >= 0x80 && Uchar.is_valid cp then cp
      else error n ("not a non-ASCII code point: " ^ s)
    else error n ("bad code point: " ^ s)
  in
  let output n s =
    if String.for_all (fun c -> c >= ' ' && c <= '~') s then s
    else error n "the output must be printable ASCII"
  in
  In_channel.with_open_text fname (fun ic ->
      let rec loop n =
        match In_channel.input_line ic with
        | None -> ()
        | Some line ->
            let line =
              if String.ends_with ~suffix:"\r" line then
                String.sub line 0 (String.length line - 1)
              else line
            in
            (if line <> "" && line.[0] <> '#' then
               match String.split_on_char '\t' line with
               | cp :: out :: _ ->
                   let cp = codepoint n cp in
                   if Hashtbl.mem table cp then error n "duplicate code point";
                   Hashtbl.replace table cp (output n out)
               | [ _ ] | [] -> error n "expected: U+XXXX<TAB>output");
            loop (n + 1)
      in
      loop 1);
  table

let write_tsv fname header rows =
  Out_channel.with_open_text fname (fun oc ->
      List.iter
        (fun row ->
          output_string oc (String.concat "\t" (List.map tsv row));
          output_char oc '\n')
        (header :: rows))

let inventory_header =
  [
    "code_point";
    "char";
    "utf8";
    "name";
    "status";
    "output";
    "candidate";
    "candidate_output";
    "fn_strings";
    "fn_persons";
    "sn_strings";
    "sn_persons";
    "examples";
  ]

let candidate override u s =
  match override with
  | None -> ("", "")
  | Some override -> (
      match override u with
      | None -> ("none", "")
      | Some t ->
          let current = fst (Name.unaccent_utf_8 true s 0) in
          ((if String.lowercase_ascii t = current then "same" else "changed"), t)
      )

let inventory_row override (cp, st) =
  let code, glyph, bytes, name, state, output, cand, cand_output =
    if cp < 0 then ("invalid", "", "", "", "invalid", "", "", "")
    else
      let u = Uchar.of_int cp in
      let s = utf_8 u in
      let state, output =
        match status u with
        | Mapped t -> ("mapped", t)
        | Removed -> ("removed", "")
        | Unmapped -> ("unmapped", "")
      in
      let cand, cand_output = candidate override u s in
      ( Printf.sprintf "U+%04X" cp,
        (if Uunf.ccc u > 0 then "\u{25CC}" ^ s else s),
        hex s,
        Uucp.Name.name u,
        state,
        output,
        cand,
        cand_output )
  in
  let examples =
    String.concat " | "
      (List.map (fun (n, s) -> Printf.sprintf "%s (%d)" s n) st.examples)
  in
  [
    code;
    glyph;
    bytes;
    name;
    state;
    output;
    cand;
    cand_output;
    string_of_int st.fn_strings;
    string_of_int st.fn_persons;
    string_of_int st.sn_strings;
    string_of_int st.sn_persons;
    examples;
  ]

let key_name fn sn occ =
  Printf.sprintf "%s.%d.%s" (Mutil.tr ' ' '_' fn) occ (Mutil.tr ' ' '_' sn)

let entry_of base override ip =
  let p = Driver.poi base ip in
  let first_name = Driver.p_first_name base p in
  let surname = Driver.p_surname base p in
  {
    iper = ip;
    first_name;
    surname;
    occ = Driver.get_occ p;
    new_fn = simulated_lower override first_name;
    new_sn = simulated_lower override surname;
  }

let compare_entries a b =
  match compare (a.new_sn, a.new_fn, a.occ) (b.new_sn, b.new_fn, b.occ) with
  | 0 -> Driver.Iper.compare a.iper b.iper
  | c -> c

let has_key e = e.new_fn <> "" && e.new_sn <> ""
let new_key e = (e.new_fn, e.new_sn, e.occ)

(* Driver.person_of_key folds its arguments with Name.lower: the new key is
   looked up only if this folding leaves it unchanged. *)
let fixed_point e =
  Name.lower e.new_fn = e.new_fn && Name.lower e.new_sn = e.new_sn

(* The persons holding each new key: the persons whose key changes, then those
   whose current key is this new key. *)
let collisions base affected entries =
  let groups = Hashtbl.create 64 in
  List.iter
    (fun e ->
      if has_key e then
        let k = new_key e in
        let mine, others =
          Option.value ~default:([], []) (Hashtbl.find_opt groups k)
        in
        let others =
          if fixed_point e then
            match Driver.person_of_key base e.new_fn e.new_sn e.occ with
            | Some ip
              when (not (Driver.Iper.Table.mem affected ip))
                   && not (List.exists (Driver.Iper.equal ip) others) ->
                ip :: others
            | Some _ | None -> others
          else others
        in
        Hashtbl.replace groups k (e :: mine, others))
    entries;
  groups

let colliding (mine, others) =
  List.compare_length_with mine 1 > 0 || List.compare_length_with others 0 > 0

let check groups e =
  if not (has_key e) then "nokey"
  else
    match Hashtbl.find_opt groups (new_key e) with
    | Some g when colliding g -> "collision"
    | Some _ | None -> if fixed_point e then "ok" else "unchecked"

let bucket_changed override e =
  let full = e.first_name ^ " " ^ e.surname in
  Name.crush_lower full
  <> Name.crush (Name.abbrev (simulated_lower override full))

let url_broken override e =
  simulated_lower override (Name.lower e.first_name) <> e.new_fn
  || simulated_lower override (Name.lower e.surname) <> e.new_sn

let key_row override groups e =
  [
    Driver.Iper.to_string e.iper;
    e.first_name;
    e.surname;
    string_of_int e.occ;
    Image.key_dir_basename e.first_name e.surname e.occ;
    key_name e.new_fn e.new_sn e.occ;
    (if bucket_changed override e then "changed" else "same");
    (if url_broken override e then "broken" else "ok");
    check groups e;
  ]

let collision_rows base groups =
  let person_row key ip fn sn occ affected =
    [ key; Driver.Iper.to_string ip; fn; sn; string_of_int occ; affected ]
  in
  Hashtbl.fold
    (fun k g acc -> if colliding g then (k, g) :: acc else acc)
    groups []
  |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
  |> List.fold_left
       (fun rows ((fn, sn, occ), (mine, others)) ->
         let key = key_name fn sn occ in
         let rows =
           List.fold_left
             (fun rows e ->
               person_row key e.iper e.first_name e.surname e.occ "yes" :: rows)
             rows (List.rev mine)
         in
         List.fold_left
           (fun rows ip ->
             let p = Driver.poi base ip in
             person_row key ip
               (Driver.p_first_name base p)
               (Driver.p_surname base p) (Driver.get_occ p) "no"
             :: rows)
           rows others)
       []
  |> List.rev

let moves ~portraits ~images ~history e =
  let old_key = Image.key_dir_basename e.first_name e.surname e.occ in
  let new_key = key_name e.new_fn e.new_sn e.occ in
  let acc = ref [] in
  let add kind o n =
    if Sys.file_exists o then acc := (kind, o, n, Sys.file_exists n) :: !acc
  in
  if old_key <> new_key then (
    List.iter
      (fun dir ->
        List.iter
          (fun (kind, suffix) ->
            Array.iter
              (fun ext ->
                add kind
                  (dir // (old_key ^ suffix ^ ext))
                  (dir // (new_key ^ suffix ^ ext)))
              Image.ext_list_2)
          [ ("portrait", ""); ("blason", ".blason") ])
      [ portraits; portraits // "saved" ];
    add "carrousel" (images // old_key) (images // new_key);
    add "history"
      (HistoryDiff.path_in history
         (HistoryDiff.history_file e.first_name e.surname e.occ))
      (HistoryDiff.path_in history new_key));
  List.rev !acc

let count f l = List.fold_left (fun n x -> if f x then n + 1 else n) 0 l

let simulate base opts report override (fn_spi, fn) (sn_spi, sn) =
  let affected = Driver.Iper.Table.create 1024 in
  let add spi istr =
    List.iter
      (fun ip -> Driver.Iper.Table.replace affected ip ())
      (Driver.spi_find spi istr)
  in
  List.iter (add fn_spi) fn.changed;
  List.iter (add sn_spi) sn.changed;
  let entries =
    Driver.Iper.Table.fold
      (fun ip () acc -> entry_of base override ip :: acc)
      affected []
    |> List.sort compare_entries
  in
  let groups = collisions base affected entries in
  let layout = Layout.of_bname ~mode:Layout.Detect opts.bname in
  let history =
    HistoryDiff.history_dir
      (Util.read_base_env layout "" false)
      (GWPARAM.bpath opts.bname)
  in
  let portraits = Layout.portraits layout in
  let images = Layout.images layout in
  let files =
    List.fold_left
      (fun acc e -> List.rev_append (moves ~portraits ~images ~history e) acc)
      [] entries
    |> List.rev
  in
  write_tsv (report "keys")
    [
      "iper";
      "first_name";
      "surname";
      "occ";
      "old_key";
      "new_key";
      "bucket";
      "old_url";
      "check";
    ]
    (List.map (key_row override groups) entries);
  let collision_rows = collision_rows base groups in
  write_tsv (report "collisions")
    [ "new_key"; "iper"; "first_name"; "surname"; "occ"; "affected" ]
    collision_rows;
  write_tsv (report "files")
    [ "kind"; "old_path"; "new_path"; "target_exists" ]
    (List.map
       (fun (kind, o, n, exists) ->
         [ kind; o; n; (if exists then "yes" else "no") ])
       files);
  Printf.eprintf
    "simulation: %d first names and %d surnames change key, %d persons (%d \
     without key, %d unchecked)\n"
    (List.length fn.changed) (List.length sn.changed) (List.length entries)
    (count (fun e -> not (has_key e)) entries)
    (count (fun e -> check groups e = "unchecked") entries);
  Printf.eprintf
    "  names.inx bucket changed: %d persons, old URL broken: %d persons -> %s\n"
    (count (bucket_changed override) entries)
    (count (url_broken override) entries)
    (report "keys");
  Printf.eprintf "  collisions: %d keys, %d persons -> %s\n"
    (Hashtbl.fold (fun _ g n -> if colliding g then n + 1 else n) groups 0)
    (List.length collision_rows)
    (report "collisions");
  Printf.eprintf "  files to rename: %d (%d targets existing) -> %s\n"
    (List.length files)
    (count (fun (_, _, _, exists) -> exists) files)
    (report "files")

let parse_cmd () =
  let bases_dir = ref (Dirs.path Secure.default_bases_dir) in
  let out = ref "." in
  let table = ref None in
  let fallback = ref false in
  let fast = ref false in
  let bname = ref "" in
  let speclist =
    [
      ( "-bd",
        Arg.Set_string bases_dir,
        Printf.sprintf "<DIR> Bases directory (default %s)."
          (Dirs.name Secure.default_bases_dir) );
      ( "-o",
        Arg.Set_string out,
        "<DIR> Directory of the reports (default: current directory)." );
      ( "-table",
        Arg.String (fun s -> table := Some s),
        "<FILE> Candidate entries of the table, one \"U+XXXX<TAB>output\" per \
         line, simulated over the current table." );
      ( "-fallback",
        Arg.Set fallback,
        " Simulate the generic folding of the characters the table does not \
         map." );
      ("-fast", Arg.Set fast, " Load the strings of the base in memory.");
    ]
    |> Arg.align
  in
  let usage = Printf.sprintf "usage: %s [options] <base>" Sys.argv.(0) in
  Arg.parse speclist
    (fun s ->
      if !bname = "" then
        bname := GWPARAM.bname_of_arg ~prog:"unidecode_audit" s
      else raise (Arg.Bad "only one base"))
    usage;
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  {
    bases_dir = !bases_dir;
    bname = !bname;
    out = !out;
    table = !table;
    fallback = !fallback;
    fast = !fast;
  }

let main () =
  let opts = parse_cmd () in
  Secure.set_bases_dir opts.bases_dir;
  let table =
    match opts.table with
    | None -> Hashtbl.create 1
    | Some fname -> (
        try read_table fname
        with Failure msg | Sys_error msg ->
          prerr_endline msg;
          exit 2)
  in
  let override =
    if Option.is_some opts.table || opts.fallback then
      Some (make_override table opts.fallback)
    else None
  in
  Driver.with_database (GWPARAM.bpath opts.bname) @@ fun base ->
  if opts.fast then Driver.load_strings_array base;
  let stats = Hashtbl.create 1024 in
  let fn_spi = Driver.persons_of_first_name base in
  let sn_spi = Driver.persons_of_surname base in
  let fn = scan_dict base stats override Fn fn_spi in
  let sn = scan_dict base stats override Sn sn_spi in
  Mutil.mkdir_p opts.out;
  let report name = opts.out // Printf.sprintf "%s.%s.tsv" opts.bname name in
  let order cp = if cp < 0 then max_int else cp in
  Hashtbl.fold (fun cp st acc -> (cp, st) :: acc) stats []
  |> List.sort (fun (a, _) (b, _) -> Int.compare (order a) (order b))
  |> List.map (inventory_row override)
  |> write_tsv (report "inventory") inventory_header;
  Printf.eprintf
    "%s: %d first names (%d with non-ASCII characters), %d surnames (%d)\n"
    opts.bname fn.entries fn.non_ascii sn.entries sn.non_ascii;
  Printf.eprintf "inventory: %d code points -> %s\n" (Hashtbl.length stats)
    (report "inventory");
  match override with
  | None -> ()
  | Some override -> (
      match List.rev_append fn.mismatches sn.mismatches with
      | _ :: _ as l ->
          Printf.eprintf
            "the simulation does not reproduce Name.lower on %d strings, e.g.:\n"
            (List.length l);
          List.iteri (fun i s -> if i < 5 then Printf.eprintf "  %s\n" s) l;
          exit 3
      | [] -> simulate base opts report override (fn_spi, fn) (sn_spi, sn))

let () =
  try main ()
  with Driver.Incompatible_format (file, found, readable) ->
    prerr_endline (Driver.incompatible_format_message file found readable);
    exit 2
