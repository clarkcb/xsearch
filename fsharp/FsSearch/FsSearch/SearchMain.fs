namespace FsSearch

open FsFindLib
open FsSearchLib

module Main =

    let HandleError (err : string) : unit =
        Logger.Log("");
        Logger.LogError err
        SearchOptions.Usage(1)

    let Search (settings : SearchSettings) : unit =
        let searcher = Searcher(settings)

        let errs = searcher.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head

        let results = searcher.Search()
        let formatter = SearchResultFormatter(settings)

        if settings.PrintResults then
            searcher.PrintResults results formatter

        if settings.PrintDirs then
            searcher.PrintMatchingDirs results formatter

        if settings.PrintFiles then
            searcher.PrintMatchingFiles results formatter

        if settings.PrintLines then
            searcher.PrintMatchingLines results formatter


    [<EntryPoint>]
    let Main(args : string[]) = 
        match (Array.toList args) with
        | [] -> HandleError "Startpath not defined"
        | _ ->
            match SearchOptions.SettingsFromArgs(args) with
            | Ok settings ->
                if settings.Debug then
                    Logger.Log settings.ToString
                if settings.PrintUsage then
                    SearchOptions.Usage(0)
                else
                    Search settings
            | Error e -> HandleError e

        // main entry point return
        0;;
