open Core.Std
open Common

let print_usage searchoptions = 
  log_msg (sprintf "\n%s\n" (Searchoptions.get_usage searchoptions));;

let print_error_with_usage (err : string) searchoptions = 
  log_msg (sprintf "\nERROR: %s\n\n%s\n" err (Searchoptions.get_usage searchoptions));;

let sort_lines (lines : string list) : string list =
  let cmp_lines l1 l2 : int =
    if l1 < l2 then -1
    else if l1 > l2 then 1
    else 0 in
  List.sort lines ~cmp:cmp_lines

let get_matching_lines (settings : Searchsettings.t) (results : Searchresult.t list) : string list =
  let rec rec_get_matching_lines (res : Searchresult.t list) (lines : string list) : string list =
    match res with
    | [] -> lines
    | r :: rs -> (
      let line = String.strip r.line in
      if settings.uniquelines && List.exists lines ~f:(fun l -> l = line)
      then rec_get_matching_lines rs lines
      else rec_get_matching_lines rs (List.append lines [line])) in
  sort_lines (rec_get_matching_lines results [])

let print_matching_lines (settings : Searchsettings.t) (results : Searchresult.t list) : unit = 
  let matching_lines = get_matching_lines settings results in
  let hdr =
    if settings.uniquelines
    then sprintf "\nUnique lines with matches (%d):" (List.length matching_lines)
    else sprintf "\nLines with matches (%d):" (List.length matching_lines) in
  log_msg hdr;
  List.iter matching_lines ~f:(fun l -> log_msg l);;

let get_matching_files (results : Searchresult.t list) : string list =
  let rec rec_get_matching_files (res : Searchresult.t list) (files : string list) : string list =
    match res with
    | [] -> files
    | r :: rs -> (
      let file = Searchfile.to_string r.file in
      if List.exists files ~f:(fun f -> f = file)
      then rec_get_matching_files rs files
      else rec_get_matching_files rs (List.append files [file])) in
  rec_get_matching_files results []

let print_matching_files (results : Searchresult.t list) : unit = 
  let matching_files = get_matching_files results in
  log_msg (sprintf "\nFiles with matches (%d):" (List.length matching_files));
  List.iter matching_files ~f:(fun f -> log_msg f);;

let get_matching_dirs (results : Searchresult.t list) : string list =
  let rec rec_get_matching_dirs (res : Searchresult.t list) (dirs : string list) : string list =
    match res with
    | [] -> dirs
    | r :: rs -> (
      let dir = r.file.path in
      if List.exists dirs ~f:(fun d -> d = dir)
      then rec_get_matching_dirs rs dirs
      else rec_get_matching_dirs rs (List.append dirs [dir])) in
  rec_get_matching_dirs results []

let print_matching_dirs (results : Searchresult.t list) : unit = 
  let matching_dirs = get_matching_dirs results in
  log_msg (sprintf "\nDirectories with matches (%d):" (List.length matching_dirs));
  List.iter matching_dirs ~f:(fun d -> log_msg d);;

let print_search_results (results : Searchresult.t list) : unit = 
  log_msg (sprintf "\nSearch results (%d):" (List.length results));
  List.iter results ~f:(fun r -> log_msg (Searchresult.to_string r));;

let search (settings : Searchsettings.t) searchoptions = 
  if settings.debug then log_msg (sprintf "settings: %s" (Searchsettings.to_string settings));
  match Searcher.search settings with
  | Ok (results : Searchresult.t list) ->
      if settings.printresults then print_search_results results;
      if settings.listdirs then print_matching_dirs results;
      if settings.listfiles then print_matching_files results;
      if settings.listlines then print_matching_lines settings results
  | Error msg -> print_error_with_usage msg searchoptions;;

let () =
  let searchoptions = Searchoptions.get_searchoptions in
  match (Array.to_list Sys.argv) with
  | []      -> print_error_with_usage "Startpath not defined" searchoptions
  | [a]     -> print_error_with_usage "Startpath not defined" searchoptions
  | _ :: tl -> (
      match (Searchoptions.settings_from_args searchoptions tl) with
      | Ok settings ->
        if settings.printusage
        then print_usage searchoptions
        else search settings searchoptions
      | Error msg -> print_error_with_usage msg searchoptions
    );;
