open Core.Std
open Xml

type file_type =
  | Unknown
  | Archive
  | Binary
  | Code
  | Text
  | Xml;;

module StringMap = Map.Make (String)

type t = (string list) StringMap.t

let empty = StringMap.empty

let to_list t = Map.to_alist t

let ws_regex = Re2.Regex.create_exn "\\s+"

let get_extensions file_type_node =
  match (List.hd (Xml.children file_type_node)) with
  | Some ext_node -> (
    match (List.hd (Xml.children ext_node)) with
    | Some exts -> Re2.Regex.split ws_regex (String.strip (Xml.pcdata exts))
    | None -> [])
  | None -> [];;

let get_file_types : (string list) StringMap.t =
  let x = Xml.parse_file (Config.xsearchpath ^ "/shared/filetypes.xml") in
  let file_type_nodes = Xml.children x in
  let fts = List.map file_type_nodes
    ~f:(fun ft -> ((Xml.attrib ft "name"), (get_extensions ft))) in
  StringMap.of_alist_exn fts;;

let is_archive t f : bool =
  List.mem (StringMap.find_exn t "archive") (Fileutil.get_extension f);;

let is_binary t f : bool =
  List.mem (StringMap.find_exn t "binary") (Fileutil.get_extension f);;

let is_code t f : bool =
  List.mem (StringMap.find_exn t "code") (Fileutil.get_extension f);;

let is_text t f : bool =
  let text_types = ["code"; "text"; "xml";] in
  List.exists text_types ~f:(fun tt -> List.mem (StringMap.find_exn t tt) (Fileutil.get_extension f));;

let is_xml t f : bool =
  List.mem (StringMap.find_exn t "xml") (Fileutil.get_extension f);;

let get_file_type t f : file_type =
  if is_text t f then Text
  else if is_binary t f then Binary
  else if is_archive t f then Archive
  else if is_code t f then Code
  else if is_xml t f then Xml
  else Unknown;;
