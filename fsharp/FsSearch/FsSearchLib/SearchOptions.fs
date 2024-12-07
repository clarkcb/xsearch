namespace FsSearchLib

open System
open System.Text.Json
open System.Text.RegularExpressions
open FsFind

module SearchOptions =   

    type SearchOption = {
        ShortArg : string;
        LongArg : string;
        Description : string
    }

    let SortOption (o1 : SearchOption) (o2 : SearchOption) : int =
        let os1 = if o1.ShortArg <> "" then o1.ShortArg + "@" + o1.LongArg else o1.LongArg
        let os2 = if o2.ShortArg <> "" then o2.ShortArg + "@" + o2.LongArg else o2.LongArg
        String.Compare(os1, os2, StringComparison.OrdinalIgnoreCase)

    let boolActionMap : Map<string, bool -> SearchSettings -> Unit> =
        [
            ("allmatches", (fun (b : bool) (settings : SearchSettings) -> settings.FirstMatch <- not b));
            ("archivesonly", (fun (b : bool) (settings : SearchSettings) -> settings.ArchivesOnly <- b));
            ("colorize", (fun (b : bool) (settings : SearchSettings) -> settings.Colorize <- b));
            ("debug", (fun (b : bool) (settings : SearchSettings) -> settings.Debug <- b));
            ("excludehidden", (fun (b : bool) (settings : SearchSettings) -> settings.IncludeHidden <- not b));
            ("firstmatch", (fun (b : bool) (settings : SearchSettings) -> settings.FirstMatch <- b));
            ("followsymlinks", (fun (b : bool) (settings : SearchSettings) -> settings.FollowSymlinks <- b));
            ("help", (fun (b : bool) (settings : SearchSettings) -> settings.PrintUsage <- b));
            ("includehidden", (fun (b : bool) (settings : SearchSettings) -> settings.IncludeHidden <- b));
            ("multilinesearch", (fun (b : bool) (settings : SearchSettings) -> settings.MultiLineSearch <- b));
            ("nocolorize", (fun (b : bool) (settings : SearchSettings) -> settings.Colorize <- not b));
            ("nofollowsymlinks", (fun (b : bool) (settings : SearchSettings) -> settings.FollowSymlinks <- not b));
            ("noprintdirs", (fun (b : bool) (settings : SearchSettings) -> settings.PrintDirs <- not b));
            ("noprintfiles", (fun (b : bool) (settings : SearchSettings) -> settings.PrintFiles <- not b));
            ("noprintlines", (fun (b : bool) (settings : SearchSettings) -> settings.PrintLines <- not b));
            ("noprintmatches", (fun (b : bool) (settings : SearchSettings) -> settings.PrintResults <- not b));
            ("norecursive", (fun (b : bool) (settings : SearchSettings) -> settings.Recursive <- not b));
            ("nosearcharchives", (fun (b : bool) (settings : SearchSettings) -> settings.SearchArchives <- not b));
            ("printdirs", (fun (b : bool) (settings : SearchSettings) -> settings.PrintDirs <- b));
            ("printfiles", (fun (b : bool) (settings : SearchSettings) -> settings.PrintFiles <- b));
            ("printlines", (fun (b : bool) (settings : SearchSettings) -> settings.PrintLines <- b));
            ("printmatches", (fun (b : bool) (settings : SearchSettings) -> settings.PrintResults <- b));
            ("recursive", (fun (b : bool) (settings : SearchSettings) -> settings.Recursive <- b));
            ("searcharchives", (fun (b : bool) (settings : SearchSettings) -> settings.SearchArchives <- b));
            ("sort-ascending", (fun (b : bool) (settings : SearchSettings) -> settings.SortDescending <- not b));
            ("sort-caseinsensitive", (fun (b : bool) (settings : SearchSettings) -> settings.SortCaseInsensitive <- b));
            ("sort-casesensitive", (fun (b : bool) (settings : SearchSettings) -> settings.SortCaseInsensitive <- not b));
            ("sort-descending", (fun (b : bool) (settings : SearchSettings) -> settings.SortDescending <- b));
            ("uniquelines", (fun (b : bool) (settings : SearchSettings) -> settings.UniqueLines <- b));
            ("verbose", (fun (b : bool) (settings : SearchSettings) -> settings.Verbose <- b));
            ("version", (fun (b : bool) (settings : SearchSettings) -> settings.PrintVersion <- b));
        ] |> Map.ofList;

    let stringActionMap : Map<string, string -> SearchSettings -> Unit> =
        [
            ("encoding", (fun (s : string) (settings : SearchSettings) -> settings.TextFileEncoding <- s ))
            ("in-archiveext", (fun (s : string) (settings : SearchSettings) -> settings.InArchiveExtensions <- settings.AddExtensions s settings.InArchiveExtensions))
            ("in-archivefilepattern", (fun (s : string) (settings : SearchSettings) -> settings.InArchiveFilePatterns <- settings.AddPattern s settings.InArchiveFilePatterns))
            ("in-dirpattern", (fun (s : string) (settings : SearchSettings) -> settings.InDirPatterns <- settings.AddPattern s settings.InDirPatterns))
            ("in-ext", (fun (s : string) (settings : SearchSettings) -> settings.InExtensions <- settings.AddExtensions s settings.InExtensions))
            ("in-filepattern", (fun (s : string) (settings : SearchSettings) -> settings.InFilePatterns <- settings.AddPattern s settings.InFilePatterns))
            ("in-filetype", (fun (s : string) (settings : SearchSettings) -> settings.InFileTypes <- settings.AddFileTypes s settings.InFileTypes))
            ("in-linesafterpattern", (fun (s : string) (settings : SearchSettings) -> settings.InLinesAfterPatterns <- settings.AddPattern s settings.InLinesAfterPatterns))
            ("in-linesbeforepattern", (fun (s : string) (settings : SearchSettings) -> settings.InLinesBeforePatterns <- settings.AddPattern s settings.InLinesBeforePatterns))
            ("linesaftertopattern", (fun (s : string) (settings : SearchSettings) -> settings.LinesAfterToPatterns <- settings.AddPattern s settings.LinesAfterToPatterns))
            ("linesafteruntilpattern", (fun (s : string) (settings : SearchSettings) -> settings.LinesAfterUntilPatterns <- settings.AddPattern s settings.LinesAfterUntilPatterns))
            ("maxlastmod", (fun (s : string) (settings : SearchSettings) -> settings.MaxLastMod <- Some(DateTime.Parse(s))));
            ("minlastmod", (fun (s : string) (settings : SearchSettings) -> settings.MinLastMod <- Some(DateTime.Parse(s))));
            ("out-archiveext", (fun (s : string) (settings : SearchSettings) -> settings.OutArchiveExtensions <- settings.AddExtensions s settings.OutArchiveExtensions))
            ("out-archivefilepattern", (fun (s : string) (settings : SearchSettings) -> settings.OutArchiveFilePatterns <- settings.AddPattern s settings.OutArchiveFilePatterns))
            ("out-dirpattern", (fun (s : string) (settings : SearchSettings) -> settings.OutDirPatterns <- settings.AddPattern s settings.OutDirPatterns))
            ("out-ext", (fun (s : string) (settings : SearchSettings) -> settings.OutExtensions <- settings.AddExtensions s settings.OutExtensions))
            ("out-filepattern", (fun (s : string) (settings : SearchSettings) -> settings.OutFilePatterns <- settings.AddPattern s settings.OutFilePatterns))
            ("out-filetype", (fun (s : string) (settings : SearchSettings) -> settings.OutFileTypes <- settings.AddFileTypes s settings.OutFileTypes))
            ("out-linesafterpattern", (fun (s : string) (settings : SearchSettings) -> settings.OutLinesAfterPatterns <- settings.AddPattern s settings.OutLinesAfterPatterns))
            ("out-linesbeforepattern", (fun (s : string) (settings : SearchSettings) -> settings.OutLinesBeforePatterns <- settings.AddPattern s settings.OutLinesBeforePatterns))
            ("path", (fun (s : string) (settings : SearchSettings) -> settings.Paths <- settings.AddPath s settings.Paths))
            ("searchpattern", (fun (s : string) (settings : SearchSettings) -> settings.SearchPatterns <- settings.AddPattern s settings.SearchPatterns))
            ("sort-by", (fun (s : string) (settings : SearchSettings) -> settings.SortBy <- SortUtil.SortByFromName s));
        ] |> Map.ofList

    let intActionMap : Map<string, int -> SearchSettings -> Unit> =
        [
            ("linesafter", (fun (i : int) (settings : SearchSettings) -> settings.LinesAfter <- i))
            ("linesbefore", (fun (i : int) (settings : SearchSettings) -> settings.LinesBefore <- i))
            ("maxdepth", (fun (i : int) (settings : SearchSettings) -> settings.MaxDepth <- i));
            ("maxlinelength", (fun (i : int) (settings : SearchSettings) -> settings.MaxLineLength <- i))
            ("maxsize", (fun (i : int) (settings : SearchSettings) -> settings.MaxSize <- i));
            ("mindepth", (fun (i : int) (settings : SearchSettings) -> settings.MinDepth <- i));
            ("minsize", (fun (i : int) (settings : SearchSettings) -> settings.MinSize <- i));
        ] |> Map.ofList

    type SearchOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,string>>>

    let OptionsFromJson (jsonString : string) : SearchOption list =
        let searchOptionsDict = JsonSerializer.Deserialize<SearchOptionsDictionary>(jsonString)
        let optionDicts = searchOptionsDict["searchoptions"]
        [ for optionDict in optionDicts do
            let longArg = optionDict["long"]
            let shortArg = if optionDict.ContainsKey("short") then optionDict["short"] else ""
            let desc = optionDict["desc"]
            yield { ShortArg=shortArg; LongArg=longArg; Description=desc } ]

    let _searchOptionsResource = FsSearchLib.EmbeddedResource.GetResourceFileContents("FsSearchLib.Resources.searchoptions.json");
    let options = OptionsFromJson(_searchOptionsResource)

    let SettingsFromArgs (args : string[]) : SearchSettings * string =
        let optionNameMap =
            let shortArgs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
            let longArgs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
            Seq.append shortArgs longArgs
            |> Map.ofSeq

        let argRegex = Regex("^(?:-{1,2})(?<opt>.*)$")
        
        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups["opt"].Value) else None

        let rec recSettingsFromArgs (argList : string list) (settings : SearchSettings) : SearchSettings * string =
            match argList with
            | [] -> settings, ""
            | head :: tail ->
                match head with
                | IsOption opt ->
                    if optionNameMap.ContainsKey(opt) then
                        let long = optionNameMap[opt]
                        if boolActionMap.ContainsKey(long) then
                            boolActionMap[long] true settings
                            if long = "help" then
                                recSettingsFromArgs [] settings
                            else
                                recSettingsFromArgs tail settings
                        elif stringActionMap.ContainsKey(long) || intActionMap.ContainsKey(long) then
                            match tail with
                            | [] ->
                                settings, $"Missing value for option: %s{opt}"
                            | aHead :: aTail ->
                                if stringActionMap.ContainsKey(long) then
                                    stringActionMap[long] aHead settings
                                else
                                    intActionMap[long] (int aHead) settings
                                recSettingsFromArgs aTail settings
                        else
                            settings, $"Invalid option: %s{opt}"
                    else
                        settings, $"Invalid option: %s{opt}"
                | _ ->
                    settings.Paths <- settings.AddPath head settings.Paths
                    recSettingsFromArgs tail settings
        let settings = SearchSettings()
        // default PrintResults to true since running as cli
        settings.PrintResults <- true
        recSettingsFromArgs (Array.toList args) settings

    let GetUsageString () : string =
        let sortedOptions = options |> List.sortWith SortOption
        let optStringMap =
            [ for opt in sortedOptions do
                let shortString : string = 
                    if opt.ShortArg <> "" then "-" + opt.ShortArg + ","
                    else ""
                let longString : string = "--" + opt.LongArg
                yield (opt.LongArg, shortString + longString) ]
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
                yield String.Format(format, optStringMap[o.LongArg], optDescMap[o.LongArg])]

        let usageString = 
            usageStrings
            |> List.append ["\nUsage:"; " fssearch [options] -s <searchpattern> <path> [<path> ...]\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) : unit =
        let usageString = GetUsageString()
        printfn $"%s{usageString}\n"
        Environment.Exit(exitCode)
