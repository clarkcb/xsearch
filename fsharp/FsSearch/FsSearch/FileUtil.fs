namespace FsSearch

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

module FileUtil = 

    let currentPath = "."
    let parentPath = ".."
    let forwardSlash = '/'
    let backSlash = '\\'
    let dirSeps = [| forwardSlash; backSlash |]
    let dotDirs = Set.ofList [
        currentPath; parentPath;
        currentPath + string forwardSlash; parentPath + string forwardSlash;
        currentPath + string backSlash; parentPath + string backSlash
    ]

    let GetHomePath () : string = 
        match Environment.GetEnvironmentVariable("HOME") with
        | home when home <> null -> home
        | _ -> Environment.GetEnvironmentVariable("USERPROFILE")

    let GetFileContents (filepath : string) (encoding : Encoding) : string =
        let contents =
            try
                use sr = new StreamReader (filepath, encoding)
                sr.ReadToEnd()
            with
            | :? IOException as e -> printfn "%s" e.Message; ""
        contents

    let GetFileLines (filePath : string) (encoding : Encoding) = seq {
        use sr = new StreamReader (filePath, encoding)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let NormalizePath (path : string) : string = 
        path.TrimEnd(dirSeps)

    let JoinPath (path1 : string) (path2 : string) : string = 
        let dirSep =
            if path1.IndexOf(backSlash) > -1 then backSlash else forwardSlash
        let p2 =
            if path2.[0] = forwardSlash || path2.[0] = backSlash then path2.Substring(1) else path2
        String.Format("{0}{1}{2}", NormalizePath path1, dirSep, p2)

    let ExpandPath (filepath : string) : string =
        if filepath.[0] = '~' then JoinPath (GetHomePath()) (filepath.Substring(1))
        else filepath

    let ContractPath (filepath : string) : string =
        if filepath.[0] = '~' then filepath 
        else filepath.Replace(GetHomePath(), "~")

    let IsDotDir (filepath : string): bool = dotDirs.Contains(filepath)

    let GetRelativePath (fullpath : string) (startpath : string) : string =
        if IsDotDir startpath
        then match NormalizePath startpath with
             | "." -> fullpath.Replace(Environment.CurrentDirectory, currentPath)
             | ".." -> fullpath.Replace(Environment.CurrentDirectory, parentPath)
             | _ -> fullpath
        else fullpath

    let IsHidden (filepath : string) : bool = 
        let startsWithDot = filepath.[0] = '.' && not (IsDotDir filepath)
        //let hasHiddenAttribute = f.Exists && (f.Attributes &&& FileAttributes.Hidden) <> 0
        startsWithDot

    let IsHiddenFile (f : FileSystemInfo) : bool = 
        (f.Name.[0] = '.' && not (IsDotDir f.Name)) ||
        (f.Exists && (f.Attributes &&& FileAttributes.Hidden) = FileAttributes.Hidden)

    let ExtensionsListFromString (exts : string) : string list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(exts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> if (x.StartsWith(".")) then x else "." + x)

;;
