namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Arguments =   

    let argOptions =
        let argList = new List<SearchArgOption>()
        argList.Add(new SearchArgOption("d", "dirname",
                                        "Specify name pattern for directories to include in search",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddInDirPattern(x)))
        argList.Add(new SearchArgOption("D", "dirfilter",
                                        "Specify name pattern for directories to exclude from search",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddOutDirPattern(x)))
        argList.Add(new SearchArgOption("f", "filename",
                                        "Specify name pattern for files to include in search",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddInFilePattern(x)))
        argList.Add(new SearchArgOption("F", "filefilter",
                                        "Specify name pattern for files to exclude from search",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddOutFilePattern(x)))
        argList.Add(new SearchArgOption("s", "search",
                                        "Specify search pattern",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddSearchPattern(x)))
        argList.Add(new SearchArgOption("x", "ext",
                                        "Specify extension for files to include in search",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddInExtension(x)))
        argList.Add(new SearchArgOption("X", "extfilter",
                                        "Specify extension for files to exclude from search",
                                        fun (x : string) (settings : SearchSettings) -> settings.AddOutExtension(x)))
        argList

    let flagOptions =
        let flagList = new List<SearchFlagOption>()
        flagList.Add(new SearchFlagOption("1", "firstmatch",
                                        "Capture only the first match for a file+search combination",
                                        fun (settings : SearchSettings) -> settings.FirstMatch = true))
        flagList.Add(new SearchFlagOption("a", "allmatches",
                                        "Capture all matches",
                                        fun (settings : SearchSettings) -> settings.FirstMatch = false))
        flagList.Add(new SearchFlagOption("", "debug",
                                        "Set output mode to debug",
                                        fun (settings : SearchSettings) -> settings.Debug = true))
        flagList.Add(new SearchFlagOption("h", "help",
                                        "Print usage and exit",
                                        fun (settings : SearchSettings) -> settings.PrintUsage = true))
        flagList.Add(new SearchFlagOption("", "listfiles",
                                        "Generate of list of matching files after searching",
                                        fun (settings : SearchSettings) -> settings.ListFiles = true))
        flagList.Add(new SearchFlagOption("", "listlines",
                                        "Generate of list of matching lines after searching",
                                        fun (settings : SearchSettings) -> settings.ListLines = true))
        flagList.Add(new SearchFlagOption("p", "printmatches",
                                        "Print matches to stdout as found*",
                                        fun (settings : SearchSettings) -> settings.PrintResults = true))
        flagList.Add(new SearchFlagOption("P", "noprintmatches",
                                        "Suppress printing of matches to stdout",
                                        fun (settings : SearchSettings) -> settings.PrintResults = false))
        flagList.Add(new SearchFlagOption("t", "dotiming",
                                        "Time search execution",
                                        fun (settings : SearchSettings) -> settings.DoTiming = true))
        flagList.Add(new SearchFlagOption("v", "verbose",
                                        "Specify verbose output",
                                        fun (settings : SearchSettings) -> settings.Verbose = true))
        flagList.Add(new SearchFlagOption("V", "version",
                                        "Print version and exit",
                                        fun (settings : SearchSettings) -> settings.PrintVersion = true))
        flagList.Add(new SearchFlagOption("z", "searchcompressed",
                                        "Search compressed files (bz2, gz, tar, zip)*",
                                        fun (settings : SearchSettings) -> settings.SearchCompressed = true))
        flagList.Add(new SearchFlagOption("Z", "nosearchcompressed",
                                        "Do not search compressed files (bz2, gz, tar, zip)",
                                        fun (settings : SearchSettings) -> settings.SearchCompressed = false))
        flagList

    let SettingsFromArgs (args : string[]) =
        let settings = new SearchSettings()

        let actionMap =
            Map.empty.
                Add("d", settings.AddInDirPattern).
                Add("D", settings.AddOutDirPattern).
                Add("f", settings.AddInFilePattern).
                Add("F", settings.AddOutFilePattern).
                Add("s", settings.AddSearchPattern).
                Add("x", settings.AddInExtension).
                Add("X", settings.AddOutExtension)

        let flagMap =
            Map.empty.
                Add("listfiles", settings.SetListFiles).
                Add("t", settings.SetDoTiming).
                Add("v", settings.SetVerbose)

        let argRegex = new Regex("^(?:-{1,2}|\/)(?<opt>.*)$")
        
        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups.["opt"].Value) else None

        let rec loop (argList:string list) =
            match argList with
            | [] -> ()
            | head::tail ->
                match head with
                | IsOption option ->
                    if (actionMap.ContainsKey(option)) then
                        match tail with
                        | [] -> printf "Error: missing value for %s arg" head
                        | aHead::aTail -> 
                            actionMap.[option] aHead
                            loop aTail
                    elif (flagMap.ContainsKey(option)) then
                        flagMap.[option] true
                        loop tail
                    else
                        printfn "Error: invalid arg: %s" head
                | _ -> settings.StartPath <- head
        loop (Array.toList args)
        settings
