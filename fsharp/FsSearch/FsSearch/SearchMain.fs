namespace FsSearch

module Main =

    let HandleError (err : string) : unit =
        Common.Log (sprintf "\nERROR: %s" err)
        SearchOptions.Usage(1)

    let Search (settings : SearchSettings.t) : unit =
        let searcher = Searcher(settings)

        let errs = searcher.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head

        searcher.Search()

        if settings.PrintResults then
            searcher.PrintResults

        if settings.ListDirs then
            searcher.PrintMatchingDirs

        if settings.ListFiles then
            searcher.PrintMatchingFiles

        if settings.ListLines then
            searcher.PrintMatchingLines


    [<EntryPoint>]
    let Main(args : string[]) = 
        match (Array.toList args) with
        | [] -> HandleError "Startpath not defined"
        | _ ->
            let settings, err = SearchOptions.SettingsFromArgs(args)

            if err.Length > 0 then
                HandleError err

            if settings.Debug then
                Common.Log (sprintf "settings: %s" (SearchSettings.ToString settings))

            if settings.PrintUsage then
                SearchOptions.Usage(0)
            else
                Search settings

        // main entry point return
        0;;
