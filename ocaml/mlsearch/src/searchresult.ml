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


(* let create (mat : Re2.Regex.Match.t) (pat : Re2.Regex.t) (line : string) (linenum : int) : t =
  let (i, l) : (int * int) = Re2.Regex.Match.get_pos_exn (`Index 0) mat in
  {
    searchpattern = Re2.Regex.pattern pat;
    file = { containers=[]; path=""; filename=""; filetype=Filetypes.Unknown; };
    linenum = linenum;
    match_start_index = i + 1;
    match_end_index = i + 1 + l;
    line = line;
    linesbefore = [];
    linesafter = [];
  };; *)

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
  let match_string =
    if sr.linenum = 0 then
      sprintf " matches at [%d:%d]" sr.match_start_index sr.match_end_index
    else
      sprintf ": %d: [%d:%d]: %s" sr.linenum sr.match_start_index sr.match_end_index sr.line in
  (Searchfile.to_string sr.file) ^ match_string;;

let to_string (sr : t) = 
  match (sr.linesbefore, sr.linesafter) with
  | ([],[]) -> single_line_to_string sr
  | _    -> multi_line_to_string sr;;
