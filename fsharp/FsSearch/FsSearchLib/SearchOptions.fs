﻿namespace FsSearchLib

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Xml.Linq

module SearchOptions =   

    type SearchOption = {
        ShortArg : string;
        LongArg : string;
        Description : string
    }

    let AddExtensions (exts : string) (extList : string list) : string list =
        List.append extList (FileUtil.ExtensionsListFromString exts)

    let AddPath (path : string) (pathList : string list) : string list =
        List.append pathList [path]

    let AddPattern (pattern : string) (patternList : Regex list) : Regex list =
        List.append patternList [Regex(pattern)]

    let FileTypesListFromString (fts : string) : FileType list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(fts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> FileTypes.FromName x)

    let AddFileTypes (fts : string) (ftList : FileType list) : FileType list =
        List.append ftList (FileTypesListFromString fts)

    let argActionMap : Map<string, string -> SearchSettings.t -> SearchSettings.t> =
        [
            ("encoding", (fun (s : string) (settings : SearchSettings.t) -> { settings with TextFileEncoding = s }));
            ("in-archiveext", (fun (s : string) (settings : SearchSettings.t) -> { settings with InArchiveExtensions = AddExtensions s settings.InArchiveExtensions }));
            ("in-archivefilepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InArchiveFilePatterns = AddPattern s settings.InArchiveFilePatterns }));
            ("in-dirpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InDirPatterns = AddPattern s settings.InDirPatterns }));
            ("in-ext", (fun (s : string) (settings : SearchSettings.t) -> { settings with InExtensions = AddExtensions s settings.InExtensions }));
            ("in-filepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InFilePatterns = AddPattern s settings.InFilePatterns }));
            ("in-filetype", (fun (s : string) (settings : SearchSettings.t) -> { settings with InFileTypes = AddFileTypes s settings.InFileTypes }));
            ("in-linesafterpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InLinesAfterPatterns = AddPattern s settings.InLinesAfterPatterns }));
            ("in-linesbeforepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with InLinesBeforePatterns = AddPattern s settings.InLinesBeforePatterns }));
            ("linesafter", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesAfter = Int32.Parse(s) }));
            ("linesaftertopattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesAfterToPatterns = AddPattern s settings.LinesAfterToPatterns }));
            ("linesafteruntilpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesAfterUntilPatterns = AddPattern s settings.LinesAfterUntilPatterns }));
            ("linesbefore", (fun (s : string) (settings : SearchSettings.t) -> { settings with LinesBefore = Int32.Parse(s) }));
            ("maxlinelength", (fun (s : string) (settings : SearchSettings.t) -> { settings with MaxLineLength = Int32.Parse(s) }));
            ("out-archiveext", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutArchiveExtensions = AddExtensions s settings.OutArchiveExtensions }));
            ("out-archivefilepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutArchiveFilePatterns = AddPattern s settings.OutArchiveFilePatterns }));
            ("out-dirpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutDirPatterns = AddPattern s settings.OutDirPatterns }));
            ("out-ext", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutExtensions = AddExtensions s settings.OutExtensions }));
            ("out-filepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutFilePatterns = AddPattern s settings.OutFilePatterns }));
            ("out-filetype", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutFileTypes = AddFileTypes s settings.OutFileTypes }));
            ("out-linesafterpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutLinesAfterPatterns = AddPattern s settings.OutLinesAfterPatterns }));
            ("out-linesbeforepattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with OutLinesBeforePatterns = AddPattern s settings.OutLinesBeforePatterns }));
            ("path", (fun (s : string) (settings : SearchSettings.t) -> { settings with Paths = AddPath s settings.Paths }));
            ("searchpattern", (fun (s : string) (settings : SearchSettings.t) -> { settings with SearchPatterns = AddPattern s settings.SearchPatterns }));
        ] |> Map.ofList

    let flagActionMap : Map<string, bool -> SearchSettings.t -> SearchSettings.t> =
        [
            ("allmatches", (fun (b : bool) (settings : SearchSettings.t) -> { settings with FirstMatch = not b }));
            ("archivesonly", (fun (b : bool) (settings : SearchSettings.t) -> SearchSettings.SetArchivesOnly b settings));
            ("colorize", (fun (b : bool) (settings : SearchSettings.t) -> { settings with Colorize = b }));
            ("debug", (fun (b : bool) (settings : SearchSettings.t) -> SearchSettings.SetDebug b settings));
            ("excludehidden", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ExcludeHidden = b }));
            ("firstmatch", (fun (b : bool) (settings : SearchSettings.t) -> { settings with FirstMatch = b }));
            ("help", (fun (b : bool) (settings : SearchSettings.t) -> { settings with PrintUsage = b }));
            ("includehidden", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ExcludeHidden = not b }));
            ("listdirs", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ListDirs = b }));
            ("listfiles", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ListFiles = b }));
            ("listlines", (fun (b : bool) (settings : SearchSettings.t) -> { settings with ListLines = b }));
            ("multilinesearch", (fun (b : bool) (settings : SearchSettings.t) -> { settings with MultiLineSearch = b }));
            ("nocolorize", (fun (b : bool) (settings : SearchSettings.t) -> { settings with Colorize = not b }));
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

    type SearchOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,string>>>

    let OptionsFromJson (jsonString : string) : SearchOption list =
        let searchOptionsDict = JsonSerializer.Deserialize<SearchOptionsDictionary>(jsonString)
        let optionDicts = searchOptionsDict.["searchoptions"]
        [ for optionDict in optionDicts do
            let longArg = optionDict.["long"]
            let shortArg = if optionDict.ContainsKey("short") then optionDict.["short"] else ""
            let desc = optionDict.["desc"]
            yield { ShortArg=shortArg; LongArg=longArg; Description=desc } ]

    let _searchOptionsResource = EmbeddedResource.GetResourceFileContents("FsSearchLib.Resources.searchoptions.json");
    let options = OptionsFromJson(_searchOptionsResource)

    let SettingsFromArgs (args : string[]) : SearchSettings.t * string =
        let optionNameMap =
            let shortargs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
            let longargs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
            Seq.append shortargs longargs
            |> Map.ofSeq

        let argRegex = Regex("^(?:-{1,2})(?<opt>.*)$")
        
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
                                settings, $"Missing value for option: %s{opt}"
                            | aHead :: aTail -> 
                                recSettingsFromArgs aTail (argActionMap.[long] aHead settings)
                        elif flagActionMap.ContainsKey(long) then
                            if long = "help" then
                                recSettingsFromArgs [] (flagActionMap.[long] true settings)
                            else
                                recSettingsFromArgs tail (flagActionMap.[long] true settings)
                        else
                            settings, $"Invalid option: %s{opt}"
                    else
                        settings, $"Invalid option: %s{opt}"
                | _ -> recSettingsFromArgs tail { settings with Paths = AddPath head settings.Paths }
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
            |> List.append ["\nUsage:"; " fssearch [options] -s <searchpattern> <path> [<path> ...]\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) : unit =
        let usageString = GetUsageString()
        printfn $"%s{usageString}\n"
        Environment.Exit(exitCode)
