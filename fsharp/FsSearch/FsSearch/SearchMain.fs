namespace FsSearch

open FsFindLib
open FsSearchLib

module Main =

    let HandleError (err : string) (colorize : bool) (searchOptions : SearchOptions) : unit =
        Logger.Log("");
        Logger.LogErrorColor err colorize
        searchOptions.Usage(1)

    let Search (config : SearchConfig) (searchOptions : SearchOptions) (settings : SearchSettings) : unit =
        let searcher = Searcher(config, settings)

        let errs = searcher.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head settings.Colorize searchOptions

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
        let config = SearchConfig()
        let searchOptions = SearchOptions(config)
        match searchOptions.SettingsFromArgs(args) with
        | Ok settings ->
            if settings.Debug then
                Logger.Log settings.ToString
            if settings.PrintUsage then
                searchOptions.Usage(0)
            else
                Search config searchOptions settings
        | Error e -> HandleError e true searchOptions

        // main entry point return
        0;;
