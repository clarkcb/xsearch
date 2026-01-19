namespace FsSearch

open FsFindLib
open FsSearchLib

module Main =

    let HandleError (err : string) (colorize : bool) : unit =
        Logger.Log("");
        Logger.LogErrorColor err colorize
        SearchOptions.Usage(1)

    let Search (settings : SearchSettings) : unit =
        let searcher = Searcher(settings)

        let errs = searcher.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head settings.Colorize

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

        if settings.PrintMatches then
            searcher.PrintMatches results formatter


    [<EntryPoint>]
    let Main(args : string[]) = 
        match SearchOptions.SettingsFromArgs(args) with
        | Ok settings ->
            if settings.Debug then
                Logger.Log settings.ToString
            if settings.PrintUsage then
                SearchOptions.Usage(0)
            else
                Search settings
        | Error e -> HandleError e true

        // main entry point return
        0;;
