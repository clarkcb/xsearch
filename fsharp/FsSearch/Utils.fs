namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Xml.Linq

module Utils = 
    let ExtensionSet (s : string) =
        let exts =
            s.Split()
            |> Array.map (fun x -> "." + x)

        let extensionSet = 
            Array.fold
                (fun (acc: Set<string>) (ext : string) -> Set.add ext acc)
                Set.empty
                exts

        extensionSet

    let PopulateFileTypes (fileStream : FileStream) =
        let fileTypesDictionary = new Dictionary<string, ISet<string>>()
        let filetypes = XDocument.Load(fileStream).Descendants(XName.Get("filetype"))
        for f in filetypes do
            let name = [for a in f.Attributes(XName.Get("name")) do yield a.Value].Head
            let extSet =  ExtensionSet [for e in f.Descendants(XName.Get("extensions")) do yield e.Value].Head
            fileTypesDictionary.Add(name, new HashSet<String>(extSet))
        let allText = new HashSet<String>(fileTypesDictionary.["text"])
        allText.UnionWith(fileTypesDictionary.["code"])
        allText.UnionWith(fileTypesDictionary.["xml"])
        if fileTypesDictionary.Remove("text") then
            fileTypesDictionary.Add("text", allText)
        let searchable = new HashSet<String>(fileTypesDictionary.["text"])
        searchable.UnionWith(fileTypesDictionary.["binary"])
        searchable.UnionWith(fileTypesDictionary.["archive"])
        fileTypesDictionary.Add("searchable", searchable)
        fileTypesDictionary

    let PopulateFileTypesFromFileInfo (fileTypesPath : FileInfo) =
        PopulateFileTypes(new FileStream(fileTypesPath.FullName, FileMode.Open))

    let PrintElapsed (name : string) (ts : TimeSpan) =
        let elapsedTime =
            String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
                ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds / 10)
        printfn "Elapsed time for %s: %s" name elapsedTime

    let PrintNames (names : string list) =
        for name in names do
            printfn "Name: %s" name

    //let SetToString (set : HashSet<string>) =
    //    let setList =
    //        [ for s in set do yield s ]
    //        |> List.sort
    //    for name in names do
    //        printfn "Name: %s" name
;;
