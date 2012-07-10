namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Main =

    [<EntryPoint>]
    let Main(args : string[]) = 
        if args.Length < 1 then Arguments.Usage(1)
        let settings = Arguments.SettingsFromArgs(args)
        if (settings.SearchPatterns.Count < 1) then Arguments.Usage(1)
        let searcher = new Searcher(settings)
        searcher.Search()
        printfn "Matches found: %d" searcher.Results.Count

        if settings.ListFiles then
            printfn "\nFiles with matches:"
            Seq.iter (fun f -> printfn "%s " (f:FileInfo).FullName) searcher.FileSet

        // main entry point return
        0;;
