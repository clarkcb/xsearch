namespace FsSearch

open FsFind
open FsSearchLib

module Main =

    let HandleError (err : string) : unit =
        Common.Log $"\nERROR: %s{err}"
        SearchOptions.Usage(1)

    let Search (settings : SearchSettings) : unit =
        let searcher = Searcher(settings)

        let errs = searcher.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head

        let results = searcher.Search()

        if settings.PrintResults then
            searcher.PrintResults results

        if settings.ListDirs then
            searcher.PrintMatchingDirs results

        if settings.ListFiles then
            searcher.PrintMatchingFiles results

        if settings.ListLines then
            searcher.PrintMatchingLines results


    [<EntryPoint>]
    let Main(args : string[]) = 
        match (Array.toList args) with
        | [] -> HandleError "Startpath not defined"
        | _ ->
            let settings, err = SearchOptions.SettingsFromArgs(args)

            if err.Length > 0 then
                HandleError err

            if settings.Debug then
                Common.Log $"settings: %s{settings.ToString}"

            if settings.PrintUsage then
                SearchOptions.Usage(0)
            else
                Search settings

        // main entry point return
        0;;
