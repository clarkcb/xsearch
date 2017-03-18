open Core.Std

let dotdirs = ["."; ".."]

let is_dotdir f : bool =
  List.mem dotdirs (Filename.basename f)

let is_hidden f : bool =
  let name = (Filename.basename f) in
  String.length name > 1 &&
  String.sub name ~pos:0 ~len:1 = "." &&
  not (List.mem dotdirs name);;

let get_extension f : string =
  match Filename.split_extension f with
  | (_, (Some ext)) -> ext
  | (_, None) -> ""
