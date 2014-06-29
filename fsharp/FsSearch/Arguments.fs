namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

module Arguments =   

    type SearchArgOption = { shortarg : string; longarg : string; description : string; action : string -> SearchSettings -> unit; }
    type SearchFlagOption = { shortarg : string; longarg : string; description : string; action : SearchSettings -> bool; }

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
            |> List.append ["\nUsage:"; " FsSearch.exe [options] <startpath>\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) =
        let usageString = GetUsageString()
        printfn "%s" usageString
        Environment.Exit(exitCode)
