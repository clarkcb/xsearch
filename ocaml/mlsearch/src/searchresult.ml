open Core.Std

type t = {
  search_pattern : string;
  file : Searchfile.t;
  line_num : int;
  match_start_index : int;
  match_end_index : int;
  line : string;
  lines_before : string list;
  lines_after : string list;
}

let create (pattern : string) (line_num : int) (start_index : int) (end_index : int) (line : string) (lines_before : string list) (lines_after : string list) : t =
  {
    search_pattern = pattern;
    file = { containers=[]; path=""; file_name=""; file_type=Filetypes.Unknown; };
    line_num = line_num;
    match_start_index = start_index;
    match_end_index = end_index;
    line = line;
    lines_before = lines_before;
    lines_after = lines_after;
  };;

let single_line_to_string (sr : t) = 
  let match_string =
    if sr.line_num = 0 then
      sprintf " matches at [%d:%d]" sr.match_start_index sr.match_end_index
    else
      sprintf ": %d: [%d:%d]: %s" sr.line_num sr.match_start_index sr.match_end_index (String.strip sr.line) in
  (Searchfile.to_string sr.file) ^ match_string;;

let multi_line_to_string (sr : t) = 
  let hdr = 
    String.concat [
      sprintf "%s\n" (String.make 80 '=');
      sprintf "%s: %d: [%d:%d]\n" (Searchfile.to_string sr.file) sr.line_num sr.match_start_index sr.match_end_index;
      sprintf "%s\n" (String.make 80 '-');
      ] in
  let max_line_num = sr.line_num + (List.length sr.lines_after) in
  let max_line_num_length = String.length (sprintf "%d" max_line_num) in
  let padded_line_num line_num = 
    let line_num_string = sprintf "%d" line_num in
    (String.make (max_line_num_length - (String.length line_num_string)) ' ') ^ line_num_string in
  let rec rec_lines lines line_num line_string = 
    match lines with
    | [] -> line_string
    | l :: ls ->
      rec_lines ls (line_num + 1) (line_string ^ (sprintf "  %s | %s\n" (padded_line_num line_num) l)) in
  let lines_before_string = rec_lines sr.lines_before (sr.line_num - (List.length sr.lines_before)) "" in
  let lines_after_string = rec_lines sr.lines_after (sr.line_num + 1) "" in
  String.concat [
    hdr;
    lines_before_string;
    sprintf "> %s | %s\n" (padded_line_num sr.line_num) sr.line;
    lines_after_string;
    ];;

let to_string (sr : t) = 
  match (sr.lines_before, sr.lines_after) with
  | ([],[]) -> single_line_to_string sr
  | _    -> multi_line_to_string sr;;
