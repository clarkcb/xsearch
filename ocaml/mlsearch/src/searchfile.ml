open Core.Std

type t = {
  containers : string list;
  path : string;
  filename : string;
  filetype : Filetypes.filetype;
}

let create (path : string) (filetype : Filetypes.filetype) : t =
  {
    containers=[];
    path=(Filename.dirname path);
    filename=(Filename.basename path);
    filetype=filetype;
  }

let to_string (sf : t) : string = 
  let container_str = 
    match sf.containers with
    | [] -> ""
    | _  -> sprintf "%s!" (String.concat sf.containers ~sep:"!") in
  sprintf "%s%s" container_str (Filename.concat sf.path sf.filename) 
;;
