﻿namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Main =

    let HandleError (err : string) : unit =
        Common.Log (sprintf "\nERROR: %s" err)
        SearchOptions.Usage(1)

    [<EntryPoint>]
    let Main(args : string[]) = 
        let settings, err = SearchOptions.SettingsFromArgs(args)

        if err.Length > 0 then
            HandleError err

        if settings.Debug then
            Common.Log (sprintf "settings: %s" (settings.ToString()))

        if settings.PrintUsage then
            SearchOptions.Usage(0)

        let searcher = new Searcher(settings)

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

        // main entry point return
        0;;
