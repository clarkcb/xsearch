namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

module SearchOptions =   

    type SearchArgOption = { shortarg : string; longarg : string; description : string; action : string -> SearchSettings -> unit; }
    type SearchFlagOption = { shortarg : string; longarg : string; description : string; action : SearchSettings -> bool; }

    let argActionMap  =
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


    let flagActionMap =
        [
            ("allmatches", (fun (settings : SearchSettings) -> settings.FirstMatch = false));
            ("archivesonly", (fun (settings : SearchSettings) -> settings.SetArchivesOnly()));
            ("debug", (fun (settings : SearchSettings) -> settings.SetDebug()));
            ("dotiming", (fun (settings : SearchSettings) -> settings.DoTiming = true));
            ("excludehidden", (fun (settings : SearchSettings) -> settings.ExcludeHidden = true));
            ("firstmatch", (fun (settings : SearchSettings) -> settings.FirstMatch = true));
            ("help", (fun (settings : SearchSettings) -> settings.PrintUsage = true));
            ("includehidden", (fun (settings : SearchSettings) -> settings.ExcludeHidden = false));
            ("listdirs", (fun (settings : SearchSettings) -> settings.ListDirs = true));
            ("listfiles", (fun (settings : SearchSettings) -> settings.ListFiles = true));
            ("listlines", (fun (settings : SearchSettings) -> settings.ListLines = true));
            ("multilinesearch", (fun (settings : SearchSettings) -> settings.MultiLineSearch = true));
            ("noprintmatches", (fun (settings : SearchSettings) -> settings.PrintResults = false));
            ("norecursive", (fun (settings : SearchSettings) -> settings.Recursive = false));
            ("nosearcharchives", (fun (settings : SearchSettings) -> settings.SearchArchives = false));
            ("printmatches", (fun (settings : SearchSettings) -> settings.PrintResults = true));
            ("recursive", (fun (settings : SearchSettings) -> settings.Recursive = true));
            ("searcharchives", (fun (settings : SearchSettings) -> settings.SearchArchives = true));
            ("uniquelines", (fun (settings : SearchSettings) -> settings.UniqueLines = true));
            ("verbose", (fun (settings : SearchSettings) -> settings.Verbose = true));
            ("version", (fun (settings : SearchSettings) -> settings.PrintVersion = true));
        ] |> Map.ofList


    let argOptions =
        [
            { SearchArgOption.shortarg = "d"; longarg = "dirname";
              description = "Specify name pattern for directories to include in search";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddInDirPattern(x) };
            { shortarg = "D"; longarg = "dirfilter";
              description = "Specify name pattern for directories to exclude from search";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddOutDirPattern(x) };
            { shortarg = "f"; longarg = "filename";
              description = "Specify name pattern for files to include in search";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddInFilePattern(x) };
            { shortarg = "F"; longarg = "filefilter";
              description = "Specify name pattern for files to exclude from search";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddOutFilePattern(x) };
            { shortarg = "s"; longarg = "search";
              description = "Specify search pattern";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddSearchPattern(x) };
            { shortarg = "x"; longarg = "ext";
              description = "Specify extension for files to include in search";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddInExtension(x) };
            { shortarg = "X"; longarg = "extfilter";
              description = "Specify extension for files to exclude from search";
              action = fun (x : string) (settings : SearchSettings) -> settings.AddOutExtension(x) };
        ]

    let flagOptions =
        [
            { SearchFlagOption.shortarg = "1"; longarg = "firstmatch";
              description = "Capture only the first match for a file+search combination";
              action = fun (settings : SearchSettings) -> settings.FirstMatch = true };
            { shortarg = "a"; longarg = "allmatches";
              description = "Capture all matches";
              action = fun (settings : SearchSettings) -> settings.FirstMatch = false };
            { shortarg = ""; longarg = "debug";
              description = "Set output mode to debug";
              action = fun (settings : SearchSettings) -> settings.Debug = true };
            { shortarg = "h"; longarg = "help";
              description = "Print usage and exit";
              action = fun (settings : SearchSettings) -> settings.PrintUsage = true };
            { shortarg = ""; longarg = "listdirs";
              description = "Generate of list of matching directories after searching";
              action = fun (settings : SearchSettings) -> settings.ListFiles = true };
            { shortarg = ""; longarg = "listfiles";
              description = "Generate of list of matching files after searching";
              action = fun (settings : SearchSettings) -> settings.ListFiles = true };
            { shortarg = ""; longarg = "listlines";
              description = "Generate of list of matching lines after searching";
              action = fun (settings : SearchSettings) -> settings.ListLines = true };
            { shortarg = "p"; longarg = "printmatches";
              description = "Print matches to stdout as found*";
              action = fun (settings : SearchSettings) -> settings.PrintResults = true };
            { shortarg = "P"; longarg = "noprintmatches";
              description = "Suppress printing of matches to stdout";
              action = fun (settings : SearchSettings) -> settings.PrintResults = false };
            { shortarg = "t"; longarg = "dotiming";
              description = "Time search execution";
              action = fun (settings : SearchSettings) -> settings.DoTiming = true };
            { shortarg = "v"; longarg = "verbose";
              description = "Specify verbose output";
              action = fun (settings : SearchSettings) -> settings.Verbose = true };
            { shortarg = "V"; longarg = "version";
              description = "Print version and exit";
              action = fun (settings : SearchSettings) -> settings.PrintVersion = true };
            { shortarg = "z"; longarg = "searcharchives";
              description = "Search archive files (bz2, gz, tar, zip)*";
              action = fun (settings : SearchSettings) -> settings.SearchArchives = true };
            { shortarg = "Z"; longarg = "nosearcharchives";
              description = "Do not search archive files (bz2, gz, tar, zip)";
              action = fun (settings : SearchSettings) -> settings.SearchArchives = false };
        ]

    let SettingsFromArgs (args : string[]) =
        let settings = new SearchSettings()

        let actionMap =
            let shortargs = seq { for opt in argOptions do if opt.shortarg <> "" then yield (opt.shortarg, opt) }
            let longargs =  seq { for opt in argOptions do yield (opt.longarg, opt) }
            Seq.append shortargs longargs
            |> Map.ofSeq

        let flagMap =
            let shortargs = seq { for opt in flagOptions do if opt.shortarg <> "" then yield (opt.shortarg, opt) }
            let longargs =  seq { for opt in flagOptions do yield (opt.longarg, opt) }
            Seq.append shortargs longargs
            |> Map.ofSeq

        let argRegex = new Regex("^(?:-{1,2}|\/)(?<opt>.*)$")
        
        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups.["opt"].Value) else None

        let rec loopArgs (argList : string list) =
            match argList with
            | [] -> ()
            | head :: tail ->
                match head with
                | IsOption option ->
                    if (actionMap.ContainsKey(option)) then
                        match tail with
                        | [] -> printf "Error: missing value for %s arg" option
                        | aHead :: aTail -> 
                            actionMap.[option].action aHead settings
                            loopArgs aTail
                    elif (flagMap.ContainsKey(option)) then
                        flagMap.[option].action settings |> ignore
                        loopArgs tail
                    else
                        printfn "Error: invalid arg: %s" option
                | _ -> settings.StartPath <- head
 
        loopArgs (Array.toList args)
        settings

    let GetUsageString () =
        let optStrings =
            let argStrings =
                [ for opt in argOptions do
                    let shortstring : String = 
                        if opt.shortarg <> "" then "-" + opt.shortarg + ","
                        else ""
                    let longstring : String = "--" + opt.longarg
                    yield shortstring + longstring ]
            let flagStrings =
                [ for opt in flagOptions do
                    let shortstring : String = 
                        if opt.shortarg <> "" then "-" + opt.shortarg + ","
                        else ""
                    let longstring : String = "--" + opt.longarg
                    yield shortstring + longstring ]
            List.append argStrings flagStrings
            |> List.sort

        let optDescs =
            let argDescs = [ for opt in argOptions -> opt.description ]
            let flagDescs = [ for opt in flagOptions -> opt.description ]
            List.append argDescs flagDescs

        let longest = 
            [for optString in optStrings -> optString.Length]
            |> List.max

        let format = " {0,-" + longest.ToString() + "}  {1}"

        let usageStrings =
            [for i in 0 .. optStrings.Length-1 do
                yield String.Format(format, optStrings.[i], optDescs.[i])]

        let usageString = 
            usageStrings
            |> List.append ["\nUsage:"; " FsSearch.exe [options] -a <searchpattern> <startpath>\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) =
        let usageString = GetUsageString()
        printfn "%s" usageString
        Environment.Exit(exitCode)
