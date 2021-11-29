namespace FsSearchLib

open System.IO

module SearchFile = 

    type t = {
        Containers : string list;
        File : FileInfo;
        FileType : FileType;
    }

    let Create (file : FileInfo) (filetype : FileType) : t =
        {
            Containers=[];
            File=file;
            FileType=filetype
        }

    let ToString (sf : t) : string =
        let container_str = 
            match sf.Containers with
            | [] -> ""
            | _  -> sprintf "%s!" (String.concat "!" sf.Containers)
        sprintf "%s%s" container_str (sf.File.ToString())
;;
