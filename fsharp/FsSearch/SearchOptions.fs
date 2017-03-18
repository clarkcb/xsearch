namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq

module SearchOptions =   

    type SearchOption = {
        ShortArg : string;
        LongArg : string;
        Description : string
    }

    let addExtensions (exts : string) (extList : string list) : string list =
        List.append extList (FileUtil.ExtensionsListFromString exts)

    let FileTypesListFromString (fts : string) : FileType list =
        let nonWord = new Regex(@"\W+")
        nonWord.Split(fts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> FileTypes.FromName x)

    let addFileTypes (fts : string) (ftList : FileType list) : FileType list =
        List.append ftList (FileTypesListFromString fts)

    let argActionMap : Map<string, string -> SearchSettings.t -> SearchSettings.t> =
        [
            ("in-archiveext", (fun (s : string) (settings : SearchSettings.t) -> { settings with InArchiveExtensions = addExtensions s settings.InArchiveExtensions }));
            ("in-archivefilepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InArchiveFilePatterns = List.append settings.InArchiveFilePatterns [new Regex(s)] }));
            ("in-dirpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InDirPatterns = List.append settings.InDirPatterns [new Regex(s)] }));
            ("in-ext", (fun (s : string) (settings : SearchSettings.t) -> { settings with InExtensions = addExtensions s settings.InExtensions }));
            ("in-filepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InFilePatterns = List.append settings.InFilePatterns [new Regex(s)] }));
            ("in-filetype", (fun (s : string) (settings : SearchSettings.t) -> { settings with InFileTypes = addFileTypes s settings.InFileTypes }));
            ("in-linesafterpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InLinesAfterPatterns = List.append settings.InLinesAfterPatterns [new Regex(s)] }));
            ("in-linesbeforepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InLinesBeforePatterns = List.append settings.InLinesBeforePatterns [new Regex(s)] }));
            ("linesafter", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesAfter = Int32.Parse(s) }));
            ("linesaftertopattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesAfterToPatterns = List.append settings.LinesAfterToPatterns [new Regex(s)] }));
            ("linesafteruntilpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesAfterUntilPatterns = List.append settings.LinesAfterUntilPatterns [new Regex(s)] }));
            ("linesbefore", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesBefore = Int32.Parse(s) }));
            ("maxlinelength", (fun (s : string) (settings : SearchSettings.t) -> { settings with MaxLineLength = Int32.Parse(s) }));
            ("out-archiveext", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutArchiveExtensions = addExtensions s settings.OutArchiveExtensions }));
            ("out-archivefilepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutArchiveFilePatterns = List.append settings.OutArchiveFilePatterns [new Regex(s)] }));
            ("out-dirpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutDirPatterns = List.append settings.OutDirPatterns [new Regex(s)] }));
            ("out-ext", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutExtensions = addExtensions s settings.OutExtensions }));
            ("out-filepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutFilePatterns = List.append settings.OutFilePatterns [new Regex(s)] }));
            ("out-filetype", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutFileTypes = addFileTypes s settings.OutFileTypes }));
            ("out-linesafterpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutLinesAfterPatterns = List.append settings.OutLinesAfterPatterns [new Regex(s)] }));
            ("out-linesbeforepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutLinesBeforePatterns = List.append settings.OutLinesBeforePatterns [new Regex(s)] }));
            ("search", (fun (s : string) (settings : SearchSettings.t) -> { settings with SearchPatterns = List.append settings.SearchPatterns [new Regex(s)] }));
        ] |> Map.ofList

    let flagActionMap : Map<string, bool -> SearchSettings.t -> SearchSettings.t> =
        [
            ("allmatches", (fun (b : bool) (settings : SearchSettings.t) -> { settings with FirstMatch = not b }));
            ("archivesonly", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ArchivesOnly = b }));
            ("debug", (fun (b : bool) (settings : SearchSettings.t) -> { settings with Debug = true; Verbose = b }));
            ("excludehidden", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ExcludeHidden = b }));
            ("firstmatch", (fun (b : bool) (settings : SearchSettings.t) -> { settings with FirstMatch = b }));
            ("help", (fun (b : bool) (settings : SearchSettings.t) -> { settings with PrintUsage = b }));
            ("includehidden", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ExcludeHidden = not b }));
            ("listdirs", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ListDirs = b }));
            ("listfiles", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ListFiles = b }));
            ("listlines", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ListLines = b }));
            ("multilinesearch", (fun (b : bool) (settings : SearchSettings.t) -> { settings with MultiLineSearch = b }));
            ("noprintmatches", (fun (b : bool) (settings : SearchSettings.t) -> { settings with PrintResults = not b }));
            ("norecursive", (fun (b : bool) (settings : SearchSettings.t) -> { settings with Recursive = not b }));
            ("nosearcharchives", (fun (b : bool) (settings : SearchSettings.t) -> { settings with SearchArchives = not b }));
            ("printmatches", (fun (b : bool) (settings : SearchSettings.t) -> { settings with PrintResults = b }));
            ("recursive", (fun (b : bool) (settings : SearchSettings.t) -> { settings with Recursive = b }));
            ("searcharchives", (fun (b : bool) (settings : SearchSettings.t) -> { settings with SearchArchives = b }));
            ("uniquelines", (fun (b : bool) (settings : SearchSettings.t) -> { settings with UniqueLines = b }));
            ("verbose", (fun (b : bool) (settings : SearchSettings.t) -> { settings with Verbose = b }));
            ("version", (fun (b : bool) (settings : SearchSettings.t) -> { settings with PrintVersion = b }));
        ] |> Map.ofList;

    let OptionsFromXml () : SearchOption list =
        let rec recOptionsFromXml (nodeList : XElement list) (options : SearchOption list) : SearchOption list =
            match nodeList with
            | [] -> options
            | n :: ns ->
                let short = [for a in n.Attributes(XName.Get("short")) do yield a.Value].Head
                let long = [for a in n.Attributes(XName.Get("long")) do yield a.Value].Head
                let desc = n.Value.Trim()
                recOptionsFromXml ns (List.append options [{ ShortArg=short; LongArg=long; Description=desc }])
        let _searchOptionsPath = Path.Combine(Config.XSEARCHPATH, "shared/searchoptions.xml")
        let _fileStream = new FileStream(FileUtil.ExpandPath(_searchOptionsPath), FileMode.Open)
        let optNodes = XDocument.Load(_fileStream).Descendants(XName.Get("searchoption"))
        recOptionsFromXml (List.ofSeq optNodes) []
        |> List.sortBy (fun o -> if (o.ShortArg <> "") then (o.ShortArg.ToLower() + "@" + o.LongArg) else o.LongArg)

    let options = OptionsFromXml()

    let SettingsFromArgs (args : string[]) : SearchSettings.t * string =
        let optionNameMap =
            let shortargs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
            let longargs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
            Seq.append shortargs longargs
            |> Map.ofSeq

        let argRegex = new Regex("^(?:-{1,2})(?<opt>.*)$")
        
        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups.["opt"].Value) else None

        let rec recSettingsFromArgs (argList : string list) (settings : SearchSettings.t) : SearchSettings.t * string =
            match argList with
            | [] -> settings, ""
            | head :: tail ->
                match head with
                | IsOption opt ->
                    if optionNameMap.ContainsKey(opt) then
                        let long = optionNameMap.[opt]
                        if argActionMap.ContainsKey(long) then
                            match tail with
                            | [] ->
                                settings, sprintf "Missing value for option: %s" opt
                            | aHead :: aTail -> 
                                recSettingsFromArgs aTail (argActionMap.[long] aHead settings)
                        elif flagActionMap.ContainsKey(long) then
                            if long = "help" then
                                recSettingsFromArgs [] (flagActionMap.[long] true settings)
                            else
                                recSettingsFromArgs tail (flagActionMap.[long] true settings)
                        else
                            settings, sprintf "Invalid option: %s" opt
                    else
                        settings, sprintf "Invalid option: %s" opt
                | _ -> recSettingsFromArgs tail { settings with StartPath = head }
        recSettingsFromArgs (Array.toList args) { SearchSettings.DefaultSettings with PrintResults = true }

    let GetUsageString () : string =
        let optStringMap =
            [ for opt in options do
                let shortstring : string = 
                    if opt.ShortArg <> "" then "-" + opt.ShortArg + ","
                    else ""
                let longstring : string = "--" + opt.LongArg
                yield (opt.LongArg, shortstring + longstring) ]
            |> Map.ofList

        let optDescMap =
            [ for opt in options do
                yield (opt.LongArg, opt.Description) ]
            |> Map.ofList

        let optStrings : string list =
            optStringMap
            |> Map.toList
            |> List.map snd

        let longest = 
            optStrings
            |> Seq.map (fun o -> o.Length)
            |> Seq.max

        let format = " {0,-" + longest.ToString() + "}  {1}"

        let usageStrings =
            [for o in options do
                yield String.Format(format, optStringMap.[o.LongArg], optDescMap.[o.LongArg])]

        let usageString = 
            usageStrings
            |> List.append ["\nUsage:"; " FsSearch.exe [options] -s <searchpattern> <startpath>\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) : unit =
        let usageString = GetUsageString()
        printfn "%s\n" usageString
        Environment.Exit(exitCode)
