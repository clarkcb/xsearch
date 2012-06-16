namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Main =

    let Usage(errCode : int) =
        printfn "\nUsage:\n"
        printfn "CsSearch.exe [options] -s \"<searchpattern>\" <startdir>\n"
        printfn "Options:"
        printfn "  -d \"<dirname_pattern>\"     Pattern for dirnames to include"
        printfn "  -D \"<dirname_pattern>\"     Pattern for dirnames to exclude"
        printfn "  -f \"<filename_pattern>\"    Pattern for filenames to include"
        printfn "  -F \"<filename_pattern>\"    Pattern for filenames to exclude"
        printfn "  --filelist                 Print list of matching files at end"
        printfn "  -t                         Time the execution"
        printfn "  -v                         Set verbose mode"
        printfn "  -x \"<ext1>[,<ext2>]\"       Extension(s) for files to include"
        printfn "  -X \"<ext1>[,<ext2>]\"       Extension(s) for files to exclude"
        printfn ""
        Environment.Exit(errCode)


    [<EntryPoint>]
    let Main(args : string[]) = 
        let settings = Arguments.SettingsFromArgs(args)
        if (settings.SearchPatterns.Count < 1) then Usage(1)
        let searcher = new Searcher(settings)
        searcher.Search()
        printfn "Matches found: %d" searcher.Results.Count

        if settings.ListFiles then
            printfn "\nFiles with matches:"
            Seq.iter (fun f -> printfn "%s " (f:FileInfo).FullName) searcher.FileSet

        // main entry point return
        0;;
