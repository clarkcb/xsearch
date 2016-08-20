namespace FsSearch

open System
open System.IO

module SearchFile = 

    type SearchFile = {
        containers : string list;
        file : FileInfo;
        filetype : FileType;
    }

    let Create (file : FileInfo) (filetype : FileType) : SearchFile =
        {
            containers=[];
            file=file;
            filetype=filetype
        }

    let ToString (sf : SearchFile) : string =
        let container_str = 
            match sf.containers with
            | [] -> ""
            | _  -> sprintf "%s!" (String.concat "!" sf.containers)
        sprintf "%s%s" container_str (sf.file.ToString())
;;
