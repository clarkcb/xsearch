open Core.Std
open Common

module Regex = Re2.Regex

(* search :: Searchsettings.t -> string list *)
let validate_settings (settings : Searchsettings.t) =
  let errs = [
    if settings.startpath = "" then (Some "Startpath not defined") else None;
    if (Sys.file_exists_exn settings.startpath) then None else (Some "Startpath not found");
    if settings.search_patterns = [] then (Some "No search patterns defined") else None;
  ] in
  List.filter errs ~f:(fun e -> Option.is_some e)
  |> List.map ~f:(fun e -> Option.value ~default:"" e);;

let is_search_dir (settings : Searchsettings.t) (dir : string) = 
  let tests : (string -> bool) list = [
    (fun d -> not (Fileutil.is_hidden d) || not settings.exclude_hidden);
    (fun d -> List.is_empty settings.in_dir_patterns ||
              List.exists settings.in_dir_patterns ~f:(fun p -> Regex.matches p d));
    (fun d -> List.is_empty settings.out_dir_patterns ||
              not (List.exists settings.out_dir_patterns ~f:(fun p -> Regex.matches p d)));
  ] in
  List.for_all tests ~f:(fun t -> t dir);;

let get_search_dirs (settings : Searchsettings.t) (path : string) = 
  let rec rec_get_search_dirs dirs search_dirs = 
    match dirs with
    | [] -> search_dirs
    | h :: t -> 
        let subdirs =
          if (is_search_dir settings h) then
            Sys.ls_dir h
              |> List.filter ~f:(fun d -> Sys.is_directory_exn (Filename.concat h d))
              |> List.map ~f:(fun d -> Filename.concat h d)
          else [] in
        let alldirs = List.append subdirs t in
        if (is_search_dir settings h)
        then rec_get_search_dirs alldirs (List.append search_dirs [h])
        else rec_get_search_dirs alldirs search_dirs in
  rec_get_search_dirs [path] [];;

let is_archive_search_file (settings : Searchsettings.t) (file : string) = 
  let tests : (string -> bool) list = [
    (* (fun f -> not (Fileutil.is_hidden f) || not settings.exclude_hidden); *)
    (fun f -> List.is_empty settings.in_archive_extensions ||
              List.mem settings.in_archive_extensions (Fileutil.get_extension f));
    (fun f -> List.is_empty settings.out_archive_extensions ||
              not (List.mem settings.out_archive_extensions (Fileutil.get_extension f)));
    (fun f -> List.is_empty settings.in_archive_file_patterns ||
              List.exists settings.in_archive_file_patterns ~f:(fun p -> Regex.matches p f));
    (fun f -> List.is_empty settings.out_archive_file_patterns ||
              not (List.exists settings.out_archive_file_patterns ~f:(fun p -> Regex.matches p f)));
  ] in
  List.for_all tests ~f:(fun t -> t file);;

let is_search_file (settings : Searchsettings.t) (file : string) = 
  let tests : (string -> bool) list = [
    (fun f -> List.is_empty settings.in_extensions ||
              List.mem settings.in_extensions (Fileutil.get_extension f));
    (fun f -> List.is_empty settings.out_extensions ||
              not (List.mem settings.out_extensions (Fileutil.get_extension f)));
    (fun f -> List.is_empty settings.in_file_patterns ||
              List.exists settings.in_file_patterns ~f:(fun p -> Regex.matches p f));
    (fun f -> List.is_empty settings.out_file_patterns ||
              not (List.exists settings.out_file_patterns ~f:(fun p -> Regex.matches p f)));
  ] in
  List.for_all tests ~f:(fun t -> t file);;

let filter_file (settings : Searchsettings.t) (sf : Searchfile.t) = 
  let file_name = (Searchfile.to_string sf) in
  if not (Fileutil.is_hidden file_name) || not settings.exclude_hidden
  then match sf.file_type with
       | Filetypes.Text -> is_search_file settings file_name
       | Filetypes.Binary -> is_search_file settings file_name
       | Filetypes.Archive -> settings.search_archive && is_archive_search_file settings file_name
       | _ -> false
  else false

let get_search_files (settings : Searchsettings.t) (file_types : Filetypes.t) (search_dirs : string list) : (Searchfile.t list) = 
  let rec rec_get_search_files dirs (search_files : Searchfile.t list) = 
    match dirs with
    | [] -> search_files
    | d :: ds -> 
      let newsearch_files = Sys.ls_dir d
        |> List.map ~f:(fun f -> Filename.concat d f)
        |> List.filter ~f:(fun f -> Sys.is_file_exn f)
        |> List.map ~f:(fun f -> Searchfile.create f (Filetypes.get_file_type file_types f))
        |> List.filter ~f:(fun sf -> filter_file settings sf)
      in
      rec_get_search_files ds (List.append search_files newsearch_files) in
  rec_get_search_files search_dirs [];;

let rec rec_get_pattern_matches (settings : Searchsettings.t) (patterns : Regex.t list) (s : string) (p_matches : (Regex.t * Regex.Match.t) list) = 
  match patterns with
  | [] -> p_matches
  | p :: ps ->
    let next_matches =
      if settings.first_match
      then Regex.get_matches_exn ~max:1 p s
      else Regex.get_matches_exn p s in
    let next_p_matches = List.map next_matches ~f:(fun m -> (p, m)) in
    rec_get_pattern_matches settings ps s (List.append p_matches next_p_matches)

let get_lines_before (settings : Searchsettings.t) (s : string) (newline_indices : int list) (start_index : int) : (string list) = 
  if (settings.lines_before > 0 && start_index > 0)
  then
    let rev_lt_indices =
      List.take_while newline_indices ~f:(fun n -> n < start_index)
      |> List.rev in
    let before_newline_indices = 
      List.take rev_lt_indices (settings.lines_before + 1)
      |> List.rev in
    let rec rec_get_lines_before nl_indices lines_before = 
      match nl_indices with
      | x :: y :: rst -> 
        let next_line = String.sub s ~pos:(x + 1) ~len:(y - x - 1) in
        rec_get_lines_before (y :: rst) (List.append lines_before [next_line])
      | _ -> lines_before in
    rec_get_lines_before before_newline_indices []
  else []

let get_lines_after (settings : Searchsettings.t) (s : string) (newline_indices : int list) (end_index : int) : (string list) = 
  if (settings.lines_after > 0 && end_index < (String.length s))
  then
    let after_newline_indices =
      List.take (List.drop_while newline_indices ~f:(fun n -> n < end_index)) (settings.lines_after + 1) in
    let rec rec_get_lines_after nl_indices lines_after = 
      match nl_indices with
      | x :: y :: rst -> 
        let next_line = String.sub s ~pos:(x + 1) ~len:(y - x - 1) in
        rec_get_lines_after (y :: rst) (List.append lines_after [next_line])
      | _ -> lines_after in
    rec_get_lines_after after_newline_indices []
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
  let p_matches = rec_get_pattern_matches settings settings.search_patterns s [] in
  let newline_indices = rec_get_newline_indices charlist 0 [] in
  (* List.iter newline_indices ~f:(fun i -> printf "%d " i); *)
  let last_index = (List.length charlist) - 1 in
  (* if settings.debug then log_msg (sprintf "\nlast_index: %d" last_index); *)
  let rec rec_line_num_for_index (nl_indices : int list) (index : int) (line_num : int) = 
    match nl_indices with
    | [] -> line_num
    | n :: ns ->
      if index <= n
      then line_num
      else rec_line_num_for_index ns index (line_num + 1) in
  let get_start_line_index line_num = 
    match line_num with
    | 1 -> 0
    | _ -> (List.nth_exn newline_indices (line_num - 2)) + 1 in
  let get_end_line_index line_num = 
    if line_num >= (List.length newline_indices)
    then last_index
    else List.nth_exn newline_indices (line_num - 1) in
  let to_search_result (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    (* if settings.debug then log_msg (sprintf "pattern: %s" pattern); *)
    let (abs_start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    (* if settings.debug then log_msg (sprintf "abs_start_index: %d" abs_start_index); *)
    (* if settings.debug then log_msg (sprintf "len: %d" len); *)
    let line_num = rec_line_num_for_index newline_indices abs_start_index 1 in
    (* if settings.debug then log_msg (sprintf "line_num: %d" line_num); *)
    let start_line_index = get_start_line_index line_num in
    (* if settings.debug then log_msg (sprintf "start_line_index: %d" start_line_index); *)

    let end_line_index = get_end_line_index line_num in
    (* if settings.debug then log_msg (sprintf "end_line_index: %d" end_line_index); *)

    let start_index = abs_start_index - start_line_index in
    (* if settings.debug then log_msg (sprintf "start_index: %d" start_index); *)
    let line = String.sub s ~pos:start_line_index ~len:(end_line_index - start_line_index) in
    (* if settings.debug then log_msg (sprintf "line: \"%s\"" line); *)
    let lines_before = get_lines_before settings s newline_indices start_line_index in
    let lines_after = get_lines_after settings s newline_indices end_line_index in
    Searchresult.create pattern line_num (start_index + 1) (start_index + 1 + len) line lines_before lines_after in
  let results : Searchresult.t list = List.map p_matches ~f:(fun (p, m) -> to_search_result p m) in
  results;;

(* search_text_file :: Searchsettings.t -> string -> Searchresult.t list *)
let search_text_file_contents (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  (* if settings.debug then log_msg (sprintf "Searching text file contents: %s" (Searchfile.to_string sf)); *)
  search_multilinestring settings (In_channel.read_all (Searchfile.to_string sf))
  |> List.map ~f:(fun r -> { r with file=sf });;

(* search_line :: Searchsettings.t -> string -> int -> Searchresult.t list *)
let search_line (settings : Searchsettings.t) (s : string) (line_num : int) (lines_before : string list) (lines_after : string list) : Searchresult.t list = 
  let to_search_result (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    let (start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    let end_index = start_index + len in
    Searchresult.create pattern line_num (start_index + 1) (end_index + 1) s lines_before lines_after in
  rec_get_pattern_matches settings settings.search_patterns s []
  |> List.map ~f:(fun (p, m) -> to_search_result p m);;

(* search_lines :: Searchsettings.t -> string -> Searchresult.t list *)
let search_lines (settings : Searchsettings.t) (lines : string list) : Searchresult.t list = 
  let rec rec_search_lines line_num lines_before lines results = 
    let next_lines =
      (* if settings.first_match && not (List.is_empty results) *)
      if settings.first_match && (List.length results) = (List.length settings.search_patterns)
      then []
      else lines in
    match next_lines with
    | [] -> results
    | l :: ls ->
      let lines_after =
        if settings.lines_after > 0
        then (List.take ls settings.lines_after)
        else [] in
      let rs = search_line settings l line_num lines_before lines_after in
      let lbs =
        if settings.lines_before > 0
        then
          if (List.length lines_before) = settings.lines_before
          then (List.append (List.drop lines_before 1) [l])
          else (List.append lines_before [l])
        else [] in
      rec_search_lines (line_num + 1) lbs ls (List.append results rs) in
  rec_search_lines 1 [] lines [];;

let search_text_file_lines (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  (* if settings.debug then log_msg (sprintf "Searching text file lines: %s" (Searchfile.to_string sf)); *)
  search_lines settings (In_channel.read_lines (Searchfile.to_string sf))
  |> List.map ~f:(fun r -> { r with file=sf });;

(* search_text_file :: Searchsettings.t -> string -> Searchresult.t list *)
let search_text_file (settings : Searchsettings.t) (sf : Searchfile.t) : Searchresult.t list = 
  if settings.debug then log_msg (sprintf "Searching text file %s" (Searchfile.to_string sf));
  if settings.multi_line_search
  then search_text_file_contents settings sf
  else search_text_file_lines settings sf;;

(* let search_binary_channel (settings : Searchsettings.t) (channel : In_channel.t) : Searchresult.t list = 
  let contents = In_channel.input_all channel in
  let p_matches = rec_get_pattern_matches settings settings.search_patterns contents [] in
  (* if settings.debug then log_msg (sprintf "p_matches: %d" (List.length p_matches)); *)
  let to_search_result (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    let (abs_start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    Searchresult.create pattern 0 (abs_start_index + 1) (abs_start_index + 1 + len) "" [] [] in
  let results : Searchresult.t list = List.map p_matches ~f:(fun (p, m) -> to_search_result p m) in
  results;; *)

let search_blob (settings : Searchsettings.t) (blob : string) : Searchresult.t list = 
  let p_matches = rec_get_pattern_matches settings settings.search_patterns blob [] in
  (* if settings.debug then log_msg (sprintf "p_matches: %d" (List.length p_matches)); *)
  let to_search_result (p : Regex.t) (m : Regex.Match.t) = 
    let pattern = Re2.Regex.pattern p in
    let (abs_start_index, len) = Regex.Match.get_pos_exn ~sub:(`Index 0) m in
    Searchresult.create pattern 0 (abs_start_index + 1) (abs_start_index + 1 + len) "" [] [] in
  let results : Searchresult.t list = List.map p_matches ~f:(fun (p, m) -> to_search_result p m) in
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
    match sf.file_type with
    | Filetypes.Text -> search_text_file settings sf
    | Filetypes.Binary -> search_binary_file settings sf
    | Filetypes.Archive -> if settings.search_archive then search_archive_file settings sf else []
    | _ -> [] in
  results;;

(* search_path :: Searchsettings.t -> Searchresult.t list *)
let search_path (settings : Searchsettings.t) (file_types : Filetypes.t) (path : string) : Searchresult.t list = 
  let search_dirs = get_search_dirs settings path in
  if settings.verbose 
  then log_msg (sprintf "\nDirectories to be searched (%d):\n%s" (List.length search_dirs) (String.concat search_dirs ~sep:"\n"));
  let search_files = get_search_files settings file_types search_dirs in
  let rec rec_search_files files results : (Searchresult.t list) = 
    match files with
    | [] -> results
    | f :: fs -> rec_search_files fs (List.append results (search_file settings f)) in
  rec_search_files search_files [];;

(* do_search :: Searchsettings.t -> Searchresult.t list *)
let do_search (settings : Searchsettings.t) : Searchresult.t list = 
  let file_types = Filetypes.get_file_types in
  if Sys.is_directory_exn settings.startpath
  then (search_path settings file_types settings.startpath)
  else
    let start_file_type = Filetypes.get_file_type file_types settings.startpath in
    let start_search_file = Searchfile.create settings.startpath start_file_type in
    search_file settings start_search_file;;

(* search :: Searchsettings.t -> Result (Searchresult.t list) *)
let search (settings : Searchsettings.t) = 
  match (validate_settings settings) with
  | [] -> Ok (do_search settings)
  | err :: _ -> Error err;;
