namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Xml.Linq

type FileType = 
    | Unknown = 0
    | Archive = 1
    | Binary  = 2
    | Code    = 3
    | Text    = 4
    | Xml     = 5

type FileTypesDictionary = Dictionary<string, List<Dictionary<string,Object>>>

type FileTypes() =
    static let archive = "archive"
    static let binary = "binary"
    static let code = "code"
    static let searchable = "searchable"
    static let text = "text"
    static let xml = "xml"

    let ExtensionSet (s : string) =
        List.fold
            (fun (acc: Set<string>) (ext : string) -> Set.add ext acc)
            Set.empty
            (FileUtil.ExtensionsListFromString s)


    let PopulateFileTypesFromJson (jsonString : string) =
        let fileTypesDictionary = Dictionary<string, ISet<string>>()
        let filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(jsonString)
        let filetypeDicts = filetypesDict.["filetypes"]
        for filetypeDict in filetypeDicts do
            let name = (filetypeDict.["type"] :?> JsonElement).GetString()
            let extensions =
                [ for x in (filetypeDict.["extensions"] :?> JsonElement).EnumerateArray() do
                    yield "." + x.GetString() ]
            fileTypesDictionary.Add(name, HashSet<String>(extensions))
        let allText = HashSet<String>(fileTypesDictionary.[text])
        allText.UnionWith(fileTypesDictionary.[code])
        allText.UnionWith(fileTypesDictionary.[xml])
        if fileTypesDictionary.Remove(text) then
            fileTypesDictionary.Add(text, allText)
        let searchableSet = HashSet<String>(fileTypesDictionary.[text])
        searchableSet.UnionWith(fileTypesDictionary.[binary])
        searchableSet.UnionWith(fileTypesDictionary.[archive])
        fileTypesDictionary.Add(searchable, searchableSet)
        fileTypesDictionary

    let PopulateFileTypesFromXml (xmlString : string) =
        let fileTypesDictionary = Dictionary<string, ISet<string>>()
        let filetypes = XDocument.Parse(xmlString).Descendants(XName.Get("filetype"))
        for f in filetypes do
            let name = [for a in f.Attributes(XName.Get("name")) do yield a.Value].Head
            let extSet =  ExtensionSet [for e in f.Descendants(XName.Get("extensions")) do yield e.Value].Head
            fileTypesDictionary.Add(name, HashSet<String>(extSet))
        let allText = HashSet<String>(fileTypesDictionary.[text])
        allText.UnionWith(fileTypesDictionary.[code])
        allText.UnionWith(fileTypesDictionary.[xml])
        if fileTypesDictionary.Remove(text) then
            fileTypesDictionary.Add(text, allText)
        let searchableSet = HashSet<String>(fileTypesDictionary.[text])
        searchableSet.UnionWith(fileTypesDictionary.[binary])
        searchableSet.UnionWith(fileTypesDictionary.[archive])
        fileTypesDictionary.Add(searchable, searchableSet)
        fileTypesDictionary

//    let _fileTypesResource = EmbeddedResource.GetResourceFileContents("FsSearch.Resources.filetypes.xml")
    let _fileTypesResource = EmbeddedResource.GetResourceFileContents("FsSearch.Resources.filetypes.json")
//    let _fileTypesDictionary = PopulateFileTypesFromXml(_fileTypesResource)
    let _fileTypesDictionary = PopulateFileTypesFromJson(_fileTypesResource)

    // read-only member properties
    member this.FileTypesDictionary = _fileTypesDictionary

    static member FromName (name : string) : FileType =
        let lname = name.ToLowerInvariant()
        if lname.Equals(text) then FileType.Text
        else if lname.Equals(binary) then FileType.Binary
        else if lname.Equals(archive) then FileType.Archive
        else if lname.Equals(code) then FileType.Code
        else if lname.Equals(xml) then FileType.Xml
        else FileType.Unknown

    static member ToName (fileType : FileType) : string =
        match fileType with
        | FileType.Archive -> "Archive"
        | FileType.Binary -> "Binary"
        | FileType.Code -> "Code"
        | FileType.Text -> "Text"
        | FileType.Xml -> "Xml"
        | _ -> "Unknown"

    member this.GetFileType (f : FileInfo) : FileType =
        if this.IsArchiveFile f then FileType.Archive
        else if this.IsBinaryFile f then FileType.Binary
        else if this.IsCodeFile f then FileType.Code
        else if this.IsXmlFile f then FileType.Xml
        else if this.IsTextFile f then FileType.Text
        else FileType.Unknown

    member this.IsArchiveFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.[archive]

    member this.IsBinaryFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.[binary]

    member this.IsCodeFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.[code]

    member this.IsSearchableFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.[searchable]

    member this.IsTextFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.[text]

    member this.IsUnknownFile (f : FileInfo) : bool =
        not (this.IsSearchableFile f)

    member this.IsXmlFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.[xml]
    ;;
