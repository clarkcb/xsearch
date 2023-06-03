open Core.Std
open Xml;;
(* 
    NOTE: xml parsing is done using 3rd-party library xml-light, which must be installed:

    $ opam install xml-light

    and a package reference must be added to the build line:

    -package xml-light
 *)
type searchOption = { long : string; short : string; desc : string };;

let get_desc option_node = 
  match (List.hd (Xml.children option_node)) with
  | Some hd -> String.strip (Xml.pcdata hd)
  | None    -> "";;

let get_search_options : searchOption list = 
  let x = Xml.parse_file (Config.xsearchpath ^ "/shared/searchoptions.xml") in
  let option_nodes = Xml.children x in
  List.map option_nodes
    ~f:(fun o -> { short=(Xml.attrib o "short"); long=(Xml.attrib o "long"); desc=(get_desc o) });;

let cmp_search_options so1 so2 = 
  let short_or_long so =
    match (so.short, so.long) with
    | ("", l) -> l
    | (s, l)  -> (String.lowercase s) ^ "@" ^ l in
  let sl1 = short_or_long so1 in
  let sl2 = short_or_long so2 in
  if sl1 < sl2 then -1
  else if sl1 > sl2 then 1
  else 0

let get_opt_strings search_options = 
  let short_and_or_long so =
    match (so.short, so.long) with
    | ("", l) -> "--" ^ l
    | (s, l)  -> "-" ^ s ^ ",--" ^ l in
  List.map search_options ~f:short_and_or_long

let pad_string s len =
  if String.length s < len then s ^ (String.make (len - (String.length s)) ' ')
  else s

let rec zip l1 l2 zipped = 
  match (l1, l2) with
  | ([], _) -> zipped
  | (_, []) -> zipped
  | ((h1 :: t1), (h2 :: t2)) -> zip t1 t2 (List.append zipped [(h1, h2)])

(* search_options_to_string :: [SearchOption] -> String *)
let search_options_to_string search_options = 
  let sorted = List.sort search_options ~cmp:cmp_search_options in
  let opt_strings = get_opt_strings sorted in
  let opt_descs = List.map sorted ~f:(fun so -> so.desc) in
  let longest = List.fold ~init:0 ~f:max (List.map opt_strings ~f:String.length) in
  let format_opt_line o d =
    " " ^ (pad_string o longest) ^ "  " ^ d in
  let zipped = zip opt_strings opt_descs [] in
  let lines = List.map zipped ~f:(fun (o, d) -> format_opt_line o d) in
  String.concat lines ~sep:"\n"

let get_usage search_options = 
  "Usage:\n mlsearch [options] -s <searchpattern> <startpath>\n\nOptions:\n"
  ^ (search_options_to_string search_options)


type argAction = string -> Searchsettings.t -> Searchsettings.t;;

let arg_actions : (string * argAction) list = [
  ("encoding", fun s ss -> { ss with text_file_encoding=s });
  ("in-archiveext", fun s ss -> { ss with in_archive_extensions=Searchsettings.add_extensions s ss.in_archive_extensions });
  ("in-archivefilepattern", fun s ss -> { ss with in_archive_file_patterns=List.append ss.in_archive_file_patterns [Re2.Regex.create_exn s] });
  ("in-dirpattern", fun s ss -> { ss with in_dir_patterns=List.append ss.in_dir_patterns [Re2.Regex.create_exn s] });
  ("in-ext", fun s ss -> { ss with in_extensions=Searchsettings.add_extensions s ss.in_extensions });
  ("in-filepattern", fun s ss -> { ss with in_file_patterns=List.append ss.in_file_patterns [Re2.Regex.create_exn s] });
  ("in-linesafterpattern", fun s ss -> { ss with in_lines_after_patterns=List.append ss.in_lines_after_patterns [Re2.Regex.create_exn s] });
  ("in-linesbeforepattern", fun s ss -> { ss with in_lines_before_patterns=List.append ss.in_lines_before_patterns [Re2.Regex.create_exn s] });
  ("linesafter", fun s ss -> { ss with lines_after=int_of_string s });
  ("linesaftertopattern", fun s ss -> { ss with lines_aftertopatterns=List.append ss.lines_aftertopatterns [Re2.Regex.create_exn s] });
  ("linesafteruntilpattern", fun s ss -> { ss with lines_afteruntilpatterns=List.append ss.lines_afteruntilpatterns [Re2.Regex.create_exn s] });
  ("linesbefore", fun s ss -> { ss with lines_before=int_of_string s });
  ("maxlinelength", fun s ss -> { ss with max_line_length=int_of_string s });
  ("out-archiveext", fun s ss -> { ss with out_archive_extensions=Searchsettings.add_extensions s ss.out_archive_extensions });
  ("out-archivefilepattern", fun s ss -> { ss with out_archive_file_patterns=List.append ss.out_archive_file_patterns [Re2.Regex.create_exn s] });
  ("out-dirpattern", fun s ss -> { ss with out_dir_patterns=List.append ss.out_dir_patterns [Re2.Regex.create_exn s] });
  ("out-ext", fun s ss -> { ss with out_extensions=Searchsettings.add_extensions s ss.out_extensions });
  ("out-filepattern", fun s ss -> { ss with out_file_patterns=List.append ss.out_file_patterns [Re2.Regex.create_exn s] });
  ("out-linesafterpattern", fun s ss -> { ss with out_lines_after_patterns=List.append ss.out_lines_after_patterns [Re2.Regex.create_exn s] });
  ("out-linesbeforepattern", fun s ss -> { ss with out_lines_before_patterns=List.append ss.out_lines_before_patterns [Re2.Regex.create_exn s] });
  ("searchpattern", fun s ss -> { ss with search_patterns=List.append ss.search_patterns [Re2.Regex.create_exn s] });
  ("startpath", fun s ss -> { ss with startpath=s })
];;

type boolFlagAction = bool -> Searchsettings.t -> Searchsettings.t;;

let bool_flag_actions : (string * boolFlagAction) list = [
  ("allmatches", fun b ss -> { ss with first_match=(not b) });
  ("archivesonly", fun b ss -> SearchSettings.set_archives_only ss b);
  ("colorize", fun b ss -> { ss with colorize=b });
  ("debug", fun b ss -> SearchSettings.set_debug ss b);
  ("excludehidden", fun b ss -> { ss with exclude_hidden=b });
  ("firstmatch", fun b ss -> { ss with first_match=b });
  ("help", fun b ss -> { ss with print_usage=b });
  ("includehidden", fun b ss -> { ss with exclude_hidden=(not b) });
  ("listdirs", fun b ss -> { ss with list_dirs=b });
  ("listfiles", fun b ss -> { ss with list_files=b });
  ("listlines", fun b ss -> { ss with list_lines=b });
  ("multilinesearch", fun b ss -> { ss with multi_line_search=b });
  ("nocolorize", fun b ss -> { ss with colorize=(not b) });
  ("noprintmatches", fun b ss -> { ss with print_results=(not b) });
  ("norecursive", fun b ss -> { ss with recursive=(not b) });
  ("nosearcharchive", fun b ss -> { ss with search_archive=(not b) });
  ("printmatches", fun b ss -> { ss with print_results=b });
  ("recursive", fun b ss -> { ss with recursive=b });
  ("searcharchive", fun b ss -> { ss with search_archive=b });
  ("uniquelines", fun b ss -> { ss with unique_lines=b });
  ("verbose", fun b ss -> { ss with verbose=b });
  ("version", fun b ss -> { ss with print_version=b })
];;

let rec arg_name arg = 
  if arg.[0] = '-' && (String.length arg) > 1 then arg_name (String.sub arg ~pos:1 ~len:((String.length arg) - 1))
  else arg

let get_long_arg search_options arg = 
  match List.find search_options ~f:(fun o -> o.long = arg || o.short = arg) with
  | Some opt -> Some opt.long
  | None     -> None

let settings_from_args search_options args = 
  let rec rec_settings_from_args (settings : Searchsettings.t) (args : string list) =
    match args with
    | [] -> Ok settings
    | hd :: tl when hd.[0] = '-' ->
        (let arg = arg_name hd in
         match get_long_arg search_options arg with
         | Some "help" -> rec_settings_from_args { settings with print_usage=true } []
         | Some long_arg ->
             (match List.find bool_flag_actions ~f:(fun (s, _) -> s = long_arg) with
              | Some (_, f) -> rec_settings_from_args (f true settings) tl
              | None ->
                (match List.find arg_actions ~f:(fun (s, _) -> s = long_arg) with
                 | Some (_, f) ->
                    (match tl with
                     | [] -> Error (sprintf "Missing value for option: %s" arg)
                     | h :: t  -> rec_settings_from_args (f h settings) t)
                 | None -> Error (sprintf "Invalid option: %s" arg)))
         | None -> Error (sprintf "Invalid option: %s" arg))
    | hd :: tl -> rec_settings_from_args { settings with Searchsettings.startpath=hd } tl in
  rec_settings_from_args { Searchsettings.default_settings with print_results=true } args;;

