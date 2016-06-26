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

let get_searchoptions : searchOption list = 
  let x = Xml.parse_file "/Users/cary/src/xsearch/shared/searchoptions.xml" in
  let option_nodes = Xml.children x in
  List.map option_nodes
    ~f:(fun o -> { short=(Xml.attrib o "short"); long=(Xml.attrib o "long"); desc=(get_desc o) });;

let cmp_searchoptions so1 so2 = 
  let short_or_long so =
    match (so.short, so.long) with
    | ("", l) -> l
    | (s, l)  -> (String.lowercase s) ^ "@" ^ l in
  let sl1 = short_or_long so1 in
  let sl2 = short_or_long so2 in
  if sl1 < sl2 then -1
  else if sl1 > sl2 then 1
  else 0

let get_opt_strings searchoptions = 
  let short_and_or_long so =
    match (so.short, so.long) with
    | ("", l) -> "--" ^ l
    | (s, l)  -> "-" ^ s ^ ",--" ^ l in
  List.map searchoptions ~f:short_and_or_long

let pad_string s len =
  if String.length s < len then s ^ (String.make (len - (String.length s)) ' ')
  else s

let rec zip l1 l2 zipped = 
  match (l1, l2) with
  | ([], _) -> zipped
  | (_, []) -> zipped
  | ((h1 :: t1), (h2 :: t2)) -> zip t1 t2 (List.append zipped [(h1, h2)])

(* searchoptions_to_string :: [SearchOption] -> String *)
let searchoptions_to_string searchoptions = 
  let sorted = List.sort searchoptions ~cmp:cmp_searchoptions in
  let opt_strings = get_opt_strings sorted in
  let opt_descs = List.map sorted ~f:(fun so -> so.desc) in
  let longest = List.fold ~init:0 ~f:max (List.map opt_strings ~f:String.length) in
  let format_opt_line o d =
    " " ^ (pad_string o longest) ^ "  " ^ d in
  let zipped = zip opt_strings opt_descs [] in
  let lines = List.map zipped ~f:(fun (o, d) -> format_opt_line o d) in
  String.concat lines ~sep:"\n"

let get_usage searchoptions = 
  "Usage:\n mlsearch [options] -s <searchpattern> <startpath>\n\nOptions:\n"
  ^ (searchoptions_to_string searchoptions)


type argAction = string -> Searchsettings.t -> Searchsettings.t;;

let arg_actions : (string * argAction) list = [
  ("in-archiveext", fun s ss -> { ss with in_archiveextensions=Searchsettings.add_extensions s ss.in_archiveextensions });
  ("in-archivefilepattern", fun s ss -> { ss with in_archivefilepatterns=List.append ss.in_archivefilepatterns [Re2.Regex.create_exn s] });
  ("in-dirpattern", fun s ss -> { ss with in_dirpatterns=List.append ss.in_dirpatterns [Re2.Regex.create_exn s] });
  ("in-ext", fun s ss -> { ss with in_extensions=Searchsettings.add_extensions s ss.in_extensions });
  ("in-filepattern", fun s ss -> { ss with in_filepatterns=List.append ss.in_filepatterns [Re2.Regex.create_exn s] });
  ("in-linesafterpattern", fun s ss -> { ss with in_linesafterpatterns=List.append ss.in_linesafterpatterns [Re2.Regex.create_exn s] });
  ("in-linesbeforepattern", fun s ss -> { ss with in_linesbeforepatterns=List.append ss.in_linesbeforepatterns [Re2.Regex.create_exn s] });
  ("linesafter", fun s ss -> { ss with linesafter=int_of_string s });
  ("linesaftertopattern", fun s ss -> { ss with linesaftertopatterns=List.append ss.linesaftertopatterns [Re2.Regex.create_exn s] });
  ("linesafteruntilpattern", fun s ss -> { ss with linesafteruntilpatterns=List.append ss.linesafteruntilpatterns [Re2.Regex.create_exn s] });
  ("linesbefore", fun s ss -> { ss with linesbefore=int_of_string s });
  ("maxlinelength", fun s ss -> { ss with maxlinelength=int_of_string s });
  ("out-archiveext", fun s ss -> { ss with out_archiveextensions=Searchsettings.add_extensions s ss.out_archiveextensions });
  ("out-archivefilepattern", fun s ss -> { ss with out_archivefilepatterns=List.append ss.out_archivefilepatterns [Re2.Regex.create_exn s] });
  ("out-dirpattern", fun s ss -> { ss with out_dirpatterns=List.append ss.out_dirpatterns [Re2.Regex.create_exn s] });
  ("out-ext", fun s ss -> { ss with out_extensions=Searchsettings.add_extensions s ss.out_extensions });
  ("out-filepattern", fun s ss -> { ss with out_filepatterns=List.append ss.out_filepatterns [Re2.Regex.create_exn s] });
  ("out-linesafterpattern", fun s ss -> { ss with out_linesafterpatterns=List.append ss.out_linesafterpatterns [Re2.Regex.create_exn s] });
  ("out-linesbeforepattern", fun s ss -> { ss with out_linesbeforepatterns=List.append ss.out_linesbeforepatterns [Re2.Regex.create_exn s] });
  ("search", fun s ss -> { ss with searchpatterns=List.append ss.searchpatterns [Re2.Regex.create_exn s] })
];;

type flagAction = Searchsettings.t -> Searchsettings.t;;

let flag_actions : (string * flagAction) list = [
  ("allmatches", fun ss -> { ss with firstmatch=false });
  ("archivesonly", fun ss -> { ss with archivesonly=true; searcharchives=true });
  ("debug", fun ss -> { ss with debug=true; verbose=true });
  ("excludehidden", fun ss -> { ss with excludehidden=true });
  ("firstmatch", fun ss -> { ss with firstmatch=true });
  ("help", fun ss -> { ss with printusage=true });
  ("includehidden", fun ss -> { ss with excludehidden=false });
  ("listdirs", fun ss -> { ss with listdirs=true });
  ("listfiles", fun ss -> { ss with listfiles=true });
  ("listlines", fun ss -> { ss with listlines=true });
  ("multilinesearch", fun ss -> { ss with multilinesearch=true });
  ("noprintmatches", fun ss -> { ss with printresults=false });
  ("norecursive", fun ss -> { ss with recursive=false });
  ("nosearcharchives", fun ss -> { ss with searcharchives=false });
  ("printmatches", fun ss -> { ss with printresults=true });
  ("recursive", fun ss -> { ss with recursive=true });
  ("searcharchives", fun ss -> { ss with searcharchives=true });
  ("uniquelines", fun ss -> { ss with uniquelines=true });
  ("verbose", fun ss -> { ss with verbose=true });
  ("version", fun ss -> { ss with printversion=true })
];;

let rec arg_name arg = 
  if arg.[0] = '-' && (String.length arg) > 1 then arg_name (String.sub arg 1 ((String.length arg) - 1))
  else arg

let get_long_arg searchoptions arg = 
  match List.find searchoptions ~f:(fun o -> o.long = arg || o.short = arg) with
  | Some opt -> Some opt.long
  | None     -> None

let settings_from_args searchoptions args = 
  let rec rec_settings_from_args searchoptions settings args =
    match args with
    | [] -> Ok settings
    | hd :: tl when hd.[0] = '-' ->
        (let arg = arg_name hd in
         match get_long_arg searchoptions arg with
         | Some long_arg ->
             (match List.find flag_actions ~f:(fun (s, f) -> s = long_arg) with
              | Some (_, f) -> rec_settings_from_args searchoptions (f settings) tl
              | None ->
                (match List.find arg_actions ~f:(fun (s, f) -> s = long_arg) with
                 | Some (_, f) ->
                    (match tl with
                     | [] -> Error (sprintf "Missing value for option: %s" arg)
                     | h :: t  -> rec_settings_from_args searchoptions (f h settings) t)
                 | None -> Error (sprintf "Invalid option: %s" arg)))
         | None -> Error (sprintf "Invalid option: %s" arg))
    | hd :: tl -> rec_settings_from_args searchoptions { settings with Searchsettings.startpath=hd } tl in
  rec_settings_from_args searchoptions Searchsettings.default_settings args;;

