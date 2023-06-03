open Core.Std

type t = {
  containers : string list;
  path : string;
  file_name : string;
  file_type : Filetypes.file_type;
}

let create (path : string) (file_type : Filetypes.file_type) : t =
  {
    containers=[];
    path=(Filename.dirname path);
    file_name=(Filename.basename path);
    file_type=file_type;
  }

let to_string (sf : t) : string = 
  let container_str = 
    match sf.containers with
    | [] -> ""
    | _  -> sprintf "%s!" (String.concat sf.containers ~sep:"!") in
  sprintf "%s%s" container_str (Filename.concat sf.path sf.file_name) 
;;
