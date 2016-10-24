open Core.Std

type t = {
  searchpattern : string;
  file : Searchfile.t;
  linenum : int;
  match_start_index : int;
  match_end_index : int;
  line : string;
  linesbefore : string list;
  linesafter : string list;
}

let create (pattern : string) (linenum : int) (start_index : int) (end_index : int) (line : string) (linesbefore : string list) (linesafter : string list) : t =
  {
    searchpattern = pattern;
    file = { containers=[]; path=""; filename=""; filetype=Filetypes.Unknown; };
    linenum = linenum;
    match_start_index = start_index;
    match_end_index = end_index;
    line = line;
    linesbefore = linesbefore;
    linesafter = linesafter;
  };;

let single_line_to_string (sr : t) = 
  let match_string =
    if sr.linenum = 0 then
      sprintf " matches at [%d:%d]" sr.match_start_index sr.match_end_index
    else
      sprintf ": %d: [%d:%d]: %s" sr.linenum sr.match_start_index sr.match_end_index (String.strip sr.line) in
  (Searchfile.to_string sr.file) ^ match_string;;

let multi_line_to_string (sr : t) = 
  let hdr = 
    String.concat [
      sprintf "%s\n" (String.make 80 '=');
      sprintf "%s: %d: [%d:%d]\n" (Searchfile.to_string sr.file) sr.linenum sr.match_start_index sr.match_end_index;
      sprintf "%s\n" (String.make 80 '-');
      ] in
  let max_linenum = sr.linenum + (List.length sr.linesafter) in
  let max_linenum_length = String.length (sprintf "%d" max_linenum) in
  let padded_linenum linenum = 
    let linenum_string = sprintf "%d" linenum in
    (String.make (max_linenum_length - (String.length linenum_string)) ' ') ^ linenum_string in
  let rec rec_lines lines linenum line_string = 
    match lines with
    | [] -> line_string
    | l :: ls ->
      rec_lines ls (linenum + 1) (line_string ^ (sprintf "  %s | %s\n" (padded_linenum linenum) l)) in
  let linesbefore_string = rec_lines sr.linesbefore (sr.linenum - (List.length sr.linesbefore)) "" in
  let linesafter_string = rec_lines sr.linesafter (sr.linenum + 1) "" in
  String.concat [
    hdr;
    linesbefore_string;
    sprintf "> %s | %s\n" (padded_linenum sr.linenum) sr.line;
    linesafter_string;
    ];;

let to_string (sr : t) = 
  match (sr.linesbefore, sr.linesafter) with
  | ([],[]) -> single_line_to_string sr
  | _    -> multi_line_to_string sr;;
