namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Resources
open System.Text.RegularExpressions
open System.Xml.Linq

type FileType = 
    | Archive = 0
    | Binary = 1
    | Text = 2
    | Unknown = 3


type FileTypes() =
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


    //let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    //let fileNames = [for f in executingAssembly.GetFiles() do yield f.Name]
    //let fileNada = Utils.PrintNames fileNames
    //let moduleNames = [for m in executingAssembly.GetModules() do yield m.Name]
    //let moduleNada = Utils.PrintNames moduleNames

    //let fileTypesFileStream = executingAssembly.GetFile(@"Resources\filestypes.xml")
    //let _fileTypesDictionary = Utils.PopulateFileTypes(fileTypesFileStream)

    //let _fileTypesPath = @"Z:\cary\src\git\xsearch\shared\filetypes.xml"
    let _fileTypesPath = "~/src/xsearch/shared/filetypes.xml"
    let _fileTypesFileInfo = new FileInfo(FileUtil.ExpandPath(_fileTypesPath))
    let _fileTypesDictionary = PopulateFileTypesFromFileInfo(_fileTypesFileInfo)

    // read-only member properties
    //member this.FileTypesPath = _fileTypesPath
    member this.FileTypesDictionary = _fileTypesDictionary

    member this.GetFileType (f : FileInfo) : FileType =
        if this.IsTextFile f then FileType.Text
        else if this.IsBinaryFile f then FileType.Binary
        else if this.IsArchiveFile f then FileType.Archive
        else FileType.Unknown

    member this.IsBinaryFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["binary"]

    member this.IsArchiveFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["archive"]

    member this.IsSearchableFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["searchable"]

    member this.IsTextFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["text"]

    member this.IsUnknownFile (f : FileInfo) =
        not (this.IsSearchableFile f)

    ;;
