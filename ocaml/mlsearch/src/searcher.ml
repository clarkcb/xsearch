open Core.Std
open Common

module Regex = Re2.Regex

(* search :: Searchsettings.t -> string list *)
let validate_settings (settings : Searchsettings.t) =
  let errs = [
    if settings.startpath = "" then (Some "Startpath not defined") else None;
    if (Sys.file_exists_exn settings.startpath) then None else (Some "Startpath not found");
    if settings.searchpatterns = [] then (Some "No search patterns defined") else None;
  ] in
  List.filter errs ~f:(fun e -> Option.is_some e)
  |> List.map ~f:(fun e -> Option.value ~default:"" e);;

let is_search_dir (settings : Searchsettings.t) (dir : string) = 
  let tests : (string -> bool) list = [
    (fun d -> not (Fileutil.is_hidden d) || not settings.excludehidden);
    (fun d -> List.is_empty settings.in_dirpatterns ||
              List.exists settings.in_dirpatterns ~f:(fun p -> Regex.matches p d));
    (fun d -> List.is_empty settings.out_dirpatterns ||
              not (List.exists settings.out_dirpatterns ~f:(fun p -> Regex.matches p d)));
  ] in
  List.for_all tests ~f:(fun t -> t dir);;

let get_search_dirs (settings : Searchsettings.t) (path : string) = 
  let rec rec_get_search_dirs dirs searchdirs = 
    match dirs with
    | [] -> searchdirs
    | h :: t -> 
        let subdirs =
          if (is_search_dir settings h) then
            Sys.ls_dir h
              |> List.filter ~f:(fun d -> Sys.is_directory_exn (Filename.concat h d))
              |> List.map ~f:(fun d -> Filename.concat h d)
          else [] in
        let alldirs = List.append subdirs t in
        if (is_search_dir settings h)
        then rec_get_search_dirs alldirs (List.append searchdirs [h])
        else rec_get_search_dirs alldirs searchdirs in
  rec_get_search_dirs [path] [];;

let is_archive_search_file (settings : Searchsettings.t) (file : string) = 
  let tests : (string -> bool) list = [
    (* (fun f -> not (Fileutil.is_hidden f) || not settings.excludehidden); *)
    (fun f -> List.is_empty settings.in_archiveextensions ||
              List.mem settings.in_archiveextensions (Fileutil.get_extension f));
    (fun f -> List.is_empty settings.out_archiveextensions ||
              not (List.mem settings.out_archiveextensions (Fileutil.get_extension f)));
    (fun f -> List.is_empty settings.in_archivefilepatterns ||
              List.exists settings.in_archivefilepatterns ~f:(fun p -> Regex.matches p f));
    (fun f -> List.is_empty settings.out_archivefilepatterns ||
              not (List.exists settings.out_archivefilepatterns ~f:(fun p -> Regex.matches p f)));
  ] in
  List.for_all tests ~f:(fun t -> t file);;

let is_search_file (settings : Searchsettings.t) (file : string) = 
  let tests : (string -> bool) list = [
    (fun f -> List.is_empty settings.in_extensions ||
              List.mem settings.in_extensions (Fileutil.get_extension f));
    (fun f -> List.is_empty settings.out_extensions ||
              not (List.mem settings.out_extensions (Fileutil.get_extension f)));
    (fun f -> List.is_empty settings.in_filepatterns ||
              List.exists settings.in_filepatterns ~f:(fun p -> Regex.matches p f));
    (fun f -> List.is_empty settings.out_filepatterns ||
              not (List.exists settings.out_filepatterns ~f:(fun p -> Regex.matches p f)));
  ] in
  List.for_all tests ~f:(fun t -> t file);;

let filter_file (settings : Searchsettings.t) (sf : Searchfile.t) = 
  let filename = (Searchfile.to_string sf) in
  if not (Fileutil.is_hidden filename) || not settings.excludehidden
  then match sf.filetype with
       | Filetypes.Text -> is_search_file settings filename
       | Filetypes.Binary -> is_search_file settings filename
       | Filetypes.Archive -> settings.searcharchives && is_archive_search_file settings filename
       | _ -> false
  else false

let get_search_files (settings : Searchsettings.t) (filetypes : Filetypes.t) (searchdirs : string list) : (Searchfile.t list) = 
  let rec rec_get_search_files dirs (searchfiles : Searchfile.t list) = 
    match dirs with
    | [] -> searchfiles
    | d :: ds -> 
      let newsearchfiles = Sys.ls_dir d
        |> List.map ~f:(fun f -> Filename.concat d f)
        |> List.filter ~f:(fun f -> Sys.is_file_exn f)
        |> List.map ~f:(fun f -> Searchfile.create f (Filetypes.get_filetype filetypes f))
        |> List.filter ~f:(fun sf -> filter_file settings sf)
      in
      rec_get_search_files ds (List.append searchfiles newsearchfiles) in
  rec_get_search_files searchdirs [];;

let rec rec_get_pattern_matches (settings : Searchsettings.t) (patterns : Regex.t list) (s : string) (p_matches : (Regex.t * Regex.Match.t) list) = 
  match patterns with
  | [] -> p_matches
  | p :: ps ->
    let next_matches =
      if settings.firstmatch
      then Regex.get_matches_exn ~max:1 p s
      else Regex.get_matches_exn p s in
    let next_p_matches = List.map next_matches ~f:(fun m -> (p, m)) in
    rec_get_pattern_matches settings ps s (List.append p_matches next_p_matches)

let get_linesbefore (settings : Searchsettings.t) (s : string) (newline_indices : int list) (start_index : int) : (string list) = 
  if (settings.linesbefore > 0 && start_index > 0)
  then
    let rev_lt_indices =
      List.take_while newline_indices ~f:(fun n -> n < start_index)
      |> List.rev in
    let before_newline_indices = 
      List.take rev_lt_indices (settings.linesbefore + 1)
      |> List.rev in
    let rec rec_get_linesbefore nl_indices linesbefore = 
      match nl_indices with
      | x :: y :: rst -> 
        let next_line = String.sub s ~pos:(x + 1) ~len:(y - x - 1) in
        rec_get_linesbefore (y :: rst) (List.append linesbefore [next_line])
      | _ -> linesbefore in
    rec_get_linesbefore before_newline_indices []
  else []

let get_linesafter (settings : Searchsettings.t) (s : string) (newline_indices : int list) (end_index : int) : (string list) = 
  if (settings.linesafter > 0 && end_index < (String.length s))
  then
    let after_newline_indices =
      List.take (List.drop_while newline_indices ~f:(fun n -> n < end_index)) (settings.linesafter + 1) in
    let rec rec_get_linesafter nl_indices linesafter = 
      match nl_indices with
      | x :: y :: rst -> 
        let next_line = String.sub s ~pos:(x + 1) ~len:(y - x - 1) in
        rec_get_linesafter (y :: rst) (List.append linesafter [next_line])
      | _ -> linesafter in
    rec_get_linesafter after_newline_indices []
  else []

let any_matches_any_pattern (lines : string list) (patterns : Regex.t list) = 
  List.exists lines ~f:(fun l -> List.exists patterns ~f:(fun p -> Regex.matches p l))

let lines_match (lines : string list) (in_patterns : Regex.t list) (out_patterns : Regex.t list) = 
  match lines with
  | [] -> if (List.is_empty in_patterns) then true else false
  | _  ->
     (((List.is_empty in_patterns) || any_matches_any_pattern lines in_patterns) &&
      (List.is_empty out_patterns) || not (any_matches_any_pattern lines out_patterns))

(* search_multilinestring :: Searchsettings.t -> string -> Searchresult.t list *)
let search_multilinestring (settings : Searchsettings.t) (s : string) : (Searchresult.t list) = 
  let charlist = String.to_list s in
  let rec rec_get_newline_indices chars curr_index newline_indices = 
    match chars with
    | [] -> newline_indices
    | c :: cs ->
      if c = '\n'
      then rec_get_newline_indices cs (curr_index + 1) (List.append newline_indices [curr_index])
      else rec_get_newline_indices cs (curr_index + 1) newline_indices in
  let p_matches = rec_get_pattern_matches settings settings.searchpatterns s [] in
  let newline_indices = rec_get_newline_indices charlist 0 [] in
  (* List.iter newline_indices ~f:(fun i -> printf "%d " i); *)
  let last_index = (List.length charlist) - 1 in
  (* if settings.debug then log_msg (sprintf "\nlast_index: %d" last_index); *)
  let rec rec_linenum_for_index (nl_indices : int list) (index : int) (linenum : int) = 
    match nl_indices with
    | [] -> linenum
    | n :: ns ->
      if index <= n
      then linenum
      else rec_linenum_for_index ns index (linenum + 1) in
  let get_start_line_index linenum = 
    match linenum with
    | 1 -> 0
    | _ -> (List.nth_exn newline_indices (linenum - 2)) + 1 in
  let get_end_line_index linenum = 
    if linenum >= (List.length newline_indices)
    then last_index
    else List.nth_exn newline_indices (linenum - 1) in
  let to_searchresult (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    (* if settings.debug then log_msg (sprintf "pattern: %s" pattern); *)
    let (abs_start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    (* if settings.debug then log_msg (sprintf "abs_start_index: %d" abs_start_index); *)
    (* if settings.debug then log_msg (sprintf "len: %d" len); *)
    let linenum = rec_linenum_for_index newline_indices abs_start_index 1 in
    (* if settings.debug then log_msg (sprintf "linenum: %d" linenum); *)
    let start_line_index = get_start_line_index linenum in
    (* if settings.debug then log_msg (sprintf "start_line_index: %d" start_line_index); *)

    let end_line_index = get_end_line_index linenum in
    (* if settings.debug then log_msg (sprintf "end_line_index: %d" end_line_index); *)

    let start_index = abs_start_index - start_line_index in
    (* if settings.debug then log_msg (sprintf "start_index: %d" start_index); *)
    let line = String.sub s ~pos:start_line_index ~len:(end_line_index - start_line_index) in
    (* if settings.debug then log_msg (sprintf "line: \"%s\"" line); *)
    let linesbefore = get_linesbefore settings s newline_indices start_line_index in
    let linesafter = get_linesafter settings s newline_indices end_line_index in
    Searchresult.create pattern linenum (start_index + 1) (start_index + 1 + len) line linesbefore linesafter in
  let results : Searchresult.t list = List.map p_matches ~f:(fun (p, m) -> to_searchresult p m) in
  results;;

(* search_text_file :: Searchsettings.t -> string -> Searchresult.t list *)
let search_text_file_contents (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  (* if settings.debug then log_msg (sprintf "Searching text file contents: %s" (Searchfile.to_string sf)); *)
  search_multilinestring settings (In_channel.read_all (Searchfile.to_string sf))
  |> List.map ~f:(fun r -> { r with file=sf });;

(* search_line :: Searchsettings.t -> string -> int -> Searchresult.t list *)
let search_line (settings : Searchsettings.t) (s : string) (linenum : int) (linesbefore : string list) (linesafter : string list) : Searchresult.t list = 
  let to_searchresult (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    let (start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    let end_index = start_index + len in
    Searchresult.create pattern linenum (start_index + 1) (end_index + 1) s linesbefore linesafter in
  rec_get_pattern_matches settings settings.searchpatterns s []
  |> List.map ~f:(fun (p, m) -> to_searchresult p m);;

(* search_lines :: Searchsettings.t -> string -> Searchresult.t list *)
let search_lines (settings : Searchsettings.t) (lines : string list) : Searchresult.t list = 
  let rec rec_search_lines linenum linesbefore lines results = 
    let next_lines =
      (* if settings.firstmatch && not (List.is_empty results) *)
      if settings.firstmatch && (List.length results) = (List.length settings.searchpatterns)
      then []
      else lines in
    match next_lines with
    | [] -> results
    | l :: ls ->
      let linesafter =
        if settings.linesafter > 0
        then (List.take ls settings.linesafter)
        else [] in
      let rs = search_line settings l linenum linesbefore linesafter in
      let lbs =
        if settings.linesbefore > 0
        then
          if (List.length linesbefore) = settings.linesbefore
          then (List.append (List.drop linesbefore 1) [l])
          else (List.append linesbefore [l])
        else [] in
      rec_search_lines (linenum + 1) lbs ls (List.append results rs) in
  rec_search_lines 1 [] lines [];;

let search_text_file_lines (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  (* if settings.debug then log_msg (sprintf "Searching text file lines: %s" (Searchfile.to_string sf)); *)
  search_lines settings (In_channel.read_lines (Searchfile.to_string sf))
  |> List.map ~f:(fun r -> { r with file=sf });;

(* search_text_file :: Searchsettings.t -> string -> Searchresult.t list *)
let search_text_file (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  if settings.debug then log_msg (sprintf "Searching text file %s" (Searchfile.to_string sf));
  if settings.multilinesearch
  then search_text_file_contents settings sf
  else search_text_file_lines settings sf;;

(* let search_binary_channel (settings : Searchsettings.t) (channel : In_channel.t) : Searchresult.t list = 
  let contents = In_channel.input_all channel in
  let p_matches = rec_get_pattern_matches settings settings.searchpatterns contents [] in
  (* if settings.debug then log_msg (sprintf "p_matches: %d" (List.length p_matches)); *)
  let to_searchresult (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    let (abs_start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    Searchresult.create pattern 0 (abs_start_index + 1) (abs_start_index + 1 + len) "" [] [] in
  let results : Searchresult.t list = List.map p_matches ~f:(fun (p, m) -> to_searchresult p m) in
  results;; *)

let search_blob (settings : Searchsettings.t) (blob : string) : Searchresult.t list = 
  let p_matches = rec_get_pattern_matches settings settings.searchpatterns blob [] in
  (* if settings.debug then log_msg (sprintf "p_matches: %d" (List.length p_matches)); *)
  let to_searchresult (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    let (abs_start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    Searchresult.create pattern 0 (abs_start_index + 1) (abs_start_index + 1 + len) "" [] [] in
  let results : Searchresult.t list = List.map p_matches ~f:(fun (p, m) -> to_searchresult p m) in
  results;;

(* search_binary_file :: Searchsettings.t -> string -> Searchresult.t list *)
let search_binary_file (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  if settings.debug then log_msg (sprintf "Searching binary file %s" (Searchfile.to_string sf));
  (* let binary_channel : In_channel.t = In_channel.create ~binary:true (Searchfile.to_string sf) in
  search_binary_channel settings binary_channel;; *)
  let blob = In_channel.read_all (Searchfile.to_string sf) in
  search_blob settings blob;;

(* search_archive_file :: Searchsettings.t -> string -> Searchresult.t list *)
let search_archive_file (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  if settings.debug then log_msg (sprintf "Searching archive file %s" (Searchfile.to_string sf));
  let results : Searchresult.t list = [] in
  results;;

(* search_file :: Searchsettings.t -> Filetypes.t -> Searchresult.t list *)
let search_file (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  (* if settings.debug then log_msg (sprintf "Searching file %s" (Searchfile.to_string sf)); *)
  let results : Searchresult.t list =
    match sf.filetype with
    | Filetypes.Text -> search_text_file settings sf
    | Filetypes.Binary -> search_binary_file settings sf
    | Filetypes.Archive -> if settings.searcharchives then search_archive_file settings sf else []
    | _ -> [] in
  results;;

(* search_path :: Searchsettings.t -> Searchresult.t list *)
let search_path (settings : Searchsettings.t) (filetypes : Filetypes.t) (path : string) : Searchresult.t list = 
  let searchdirs = get_search_dirs settings path in
  if settings.verbose 
  then log_msg (sprintf "\nDirectories to be searched (%d):\n%s" (List.length searchdirs) (String.concat searchdirs ~sep:"\n"));
  let searchfiles = get_search_files settings filetypes searchdirs in
  let rec rec_search_files files results : (Searchresult.t list) = 
    match files with
    | [] -> results
    | f :: fs -> rec_search_files fs (List.append results (search_file settings f)) in
  rec_search_files searchfiles [];;

(* do_search :: Searchsettings.t -> Searchresult.t list *)
let do_search (settings : Searchsettings.t) : Searchresult.t list = 
  let filetypes = Filetypes.get_filetypes in
  if Sys.is_directory_exn settings.startpath
  then (search_path settings filetypes settings.startpath)
  else
    let start_filetype = Filetypes.get_filetype filetypes settings.startpath in
    let start_searchfile = Searchfile.create settings.startpath start_filetype in
    search_file settings start_searchfile;;

(* search :: Searchsettings.t -> Result (Searchresult.t list) *)
let search (settings : Searchsettings.t) = 
  match (validate_settings settings) with
  | [] -> Ok (do_search settings)
  | err :: _ -> Error err;;
