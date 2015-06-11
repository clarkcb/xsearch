namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq

module SearchOptions =   

    type SearchOption (shortarg : string, longarg : string, desc : string) =
        let _shortarg = shortarg
        let _longarg = longarg
        let _desc = desc

        // read-only member properties
        member this.ShortArg = _shortarg
        member this.LongArg = _longarg
        member this.Description = _desc
        member this.SortArg =
            if not (String.IsNullOrWhiteSpace _shortarg) then
                sprintf "%sa%s" (_shortarg.ToLower()) (_longarg)
            else
                longarg

    let argActionMap =
        [
            ("in-archiveext", (fun (s : string) (settings : SearchSettings) -> settings.AddInArchiveExtension(s)));
            ("in-archivefilepattern", (fun (s : string) (settings : SearchSettings) -> settings.AddInArchiveFilePattern(s)));
            ("in-dirpattern", (fun (s : string) (settings : SearchSettings) -> settings.AddInDirPattern(s)));
            ("in-ext", (fun (s : string) (settings : SearchSettings) -> settings.AddInExtension(s)));
            ("in-filepattern", (fun (s : string) (settings : SearchSettings) -> settings.AddInFilePattern(s)));
            ("in-linesafterpattern", (fun (s : string) (settings : SearchSettings) -> settings.AddInLinesAfterPattern(s)));
            ("in-linesbeforepattern", (fun (s : string) (settings : SearchSettings) -> settings.AddInLinesBeforePattern(s)));
            ("linesafter", (fun (s : string) (settings : SearchSettings) -> settings.LinesAfter = Int32.Parse(s) |> ignore));
            ("linesaftertopattern", (fun (s : string) (settings : SearchSettings) -> settings.AddLinesAfterToPattern(s)));
            ("linesafteruntilpattern", (fun (s : string) (settings : SearchSettings) -> settings.AddLinesAfterUntilPattern(s)));
            ("linesbefore", (fun (s : string) (settings : SearchSettings) -> settings.LinesBefore = Int32.Parse(s) |> ignore));
            ("maxlinelength", (fun (s : string) (settings : SearchSettings) -> settings.MaxLineLength = Int32.Parse(s) |> ignore));
            ("out-archiveext", (fun (s : string) (settings : SearchSettings) -> settings.AddOutArchiveExtension(s)));
            ("out-archivefilepattern", (fun (s : string) (settings : SearchSettings) -> settings.AddOutArchiveFilePattern(s)));
            ("out-dirpattern", (fun (s : string) (settings : SearchSettings) -> settings.AddOutDirPattern(s)));
            ("out-ext", (fun (s : string) (settings : SearchSettings) -> settings.AddOutExtension(s)));
            ("out-filepattern", (fun (s : string) (settings : SearchSettings) -> settings.AddOutFilePattern(s)));
            ("out-linesafterpattern", (fun (s : string) (settings : SearchSettings) -> settings.AddOutLinesAfterPattern(s)));
            ("out-linesbeforepattern", (fun (s : string) (settings : SearchSettings) -> settings.AddOutLinesBeforePattern(s)));
            ("search", (fun (s : string) (settings : SearchSettings) -> settings.AddSearchPattern(s)));
        ] |> Map.ofList

    let flagActionMap : Map<string, SearchSettings -> unit> =
        [
            ("allmatches", (fun (settings : SearchSettings) -> settings.FirstMatch <- false));
            ("archivesonly", (fun (settings : SearchSettings) -> settings.SetArchivesOnly()));
            ("debug", (fun (settings : SearchSettings) -> settings.SetDebug()));
            ("dotiming", (fun (settings : SearchSettings) -> settings.DoTiming <- true));
            ("excludehidden", (fun (settings : SearchSettings) -> settings.ExcludeHidden <- true));
            ("firstmatch", (fun (settings : SearchSettings) -> settings.FirstMatch <- true));
            ("help", (fun (settings : SearchSettings) -> settings.PrintUsage <- true));
            ("includehidden", (fun (settings : SearchSettings) -> settings.ExcludeHidden <- false));
            ("listdirs", (fun (settings : SearchSettings) -> settings.ListDirs <- true));
            ("listfiles", (fun (settings : SearchSettings) -> settings.ListFiles <- true));
            ("listlines", (fun (settings : SearchSettings) -> settings.ListLines <- true));
            ("multilinesearch", (fun (settings : SearchSettings) -> settings.MultiLineSearch <- true));
            ("noprintmatches", (fun (settings : SearchSettings) -> settings.PrintResults <- false));
            ("norecursive", (fun (settings : SearchSettings) -> settings.Recursive <- false));
            ("nosearcharchives", (fun (settings : SearchSettings) -> settings.SearchArchives <- false));
            ("printmatches", (fun (settings : SearchSettings) -> settings.PrintResults <- true));
            ("recursive", (fun (settings : SearchSettings) -> settings.Recursive <- true));
            ("searcharchives", (fun (settings : SearchSettings) -> settings.SearchArchives <- true));
            ("uniquelines", (fun (settings : SearchSettings) -> settings.UniqueLines <- true));
            ("verbose", (fun (settings : SearchSettings) -> settings.Verbose <- true));
            ("version", (fun (settings : SearchSettings) -> settings.PrintVersion <- true));
        ] |> Map.ofList;

    let OptionsFromXml () =
        let _searchOptionsPath = "~/src/xsearch/shared/searchoptions.xml"
        let _fileStream = new FileStream(FileUtil.ExpandPath(_searchOptionsPath), FileMode.Open)
        let options = new List<SearchOption>()
        let optNodes = XDocument.Load(_fileStream).Descendants(XName.Get("searchoption"))
        for o in optNodes do
            let short = [for a in o.Attributes(XName.Get("short")) do yield a.Value].Head
            let long = [for a in o.Attributes(XName.Get("long")) do yield a.Value].Head
            let desc = o.Value.Trim()
            options.Add(new SearchOption(short, long, desc))
        options |> List.ofSeq |> List.sortBy (fun o -> o.SortArg)

    let options = OptionsFromXml()

    let SettingsFromArgs (args : string[]) =
        let settings = new SearchSettings()
        settings.PrintResults <- true

        let optionNameMap =
            let shortargs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
            let longargs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
            Seq.append shortargs longargs
            |> Map.ofSeq

        let argRegex = new Regex("^(?:-{1,2})(?<opt>.*)$")
        
        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups.["opt"].Value) else None

        let rec loopArgs (argList : string list) =
            match argList with
            | [] -> ()
            | head :: tail ->
                match head with
                | IsOption opt ->
                    let long = optionNameMap.[opt]
                    if (argActionMap.ContainsKey(long)) then
                        match tail with
                        | [] -> printf "Error: missing value for %s arg" opt
                        | aHead :: aTail -> 
                            argActionMap.[long] aHead settings
                            loopArgs aTail
                    elif (flagActionMap.ContainsKey(long)) then
                        flagActionMap.[long] settings
                        loopArgs tail
                    else
                        printfn "Error: invalid arg: %s" opt
                | _ -> settings.StartPath <- head
                       loopArgs tail
        loopArgs (Array.toList args)
        settings

    let GetUsageString () =
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

    let Usage (exitCode : int) =
        let usageString = GetUsageString()
        printfn "%s\n" usageString
        Environment.Exit(exitCode)
