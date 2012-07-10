namespace FsSearch

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Searcher (settings : SearchSettings) =
    let _settings = settings
    let _fileUtil = new FileUtil()
    let _results = new List<SearchResult>()
    let _fileSet = new HashSet<FileInfo>()
    let _timers = new Dictionary<string,Stopwatch>()

    // read-only member properties
    member this.Settings = _settings
    member this.Results = _results
    member this.FileSet = _fileSet
    member this.Timers = _timers

    // member methods
    member this.IsTargetDirectory (d : DirectoryInfo) =
        (Seq.isEmpty _settings.InDirPatterns ||
         Seq.exists (fun p -> (p:Regex).Match(d.FullName).Success) _settings.InDirPatterns) &&
        (Seq.isEmpty _settings.OutDirPatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(d.FullName).Success) _settings.OutDirPatterns))

    member this.IsTargetFile (f : FileInfo) =
        (Seq.isEmpty _settings.InExtensions ||
         Seq.exists (fun x -> x = f.Extension) _settings.InExtensions) &&
        (Seq.isEmpty _settings.OutExtensions ||
         not (Seq.exists (fun x -> x = f.Extension) _settings.OutExtensions)) &&
        (Seq.isEmpty _settings.InFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(f.Name).Success) _settings.InFilePatterns) &&
        (Seq.isEmpty _settings.OutFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(f.Name).Success) _settings.OutFilePatterns))

    member this.StartTimer (name : string) =
        let timer = new Stopwatch()
        timer.Start()
        _timers.Add(name, timer)

    member this.StopTimer (name : string) =
        let timer = _timers.[name]
        timer.Stop()
        Utils.PrintElapsed name timer.Elapsed

    member this.GetSearchFiles (dir : DirectoryInfo) =
        try
            let files =
                dir.EnumerateFiles()
                |> Seq.filter (fun f -> this.IsTargetFile (f:FileInfo))
            let dirs =
                dir.EnumerateDirectories()
                |> Seq.filter (fun d -> this.IsTargetDirectory (d:DirectoryInfo))
            Seq.fold (fun files d -> (Seq.concat [files; (this.GetSearchFiles d)])) files dirs
        with
        | :? System.IO.IOException as e ->
            printfn "Error while accessing dir %s: %s" dir.FullName e.Message
            Seq.empty

    member this.Search () =
        let startDir = new DirectoryInfo(_settings.StartPath)
        if _settings.Verbose then
            printfn "Starting directory: %s" startDir.FullName
        if not startDir.Exists then
            raise <| FileNotFoundException ("File not found", startDir.FullName)
        if not (this.IsTargetDirectory startDir) then
            raise <| Exception ("Starting directory matches an exclusion filter or does not match an inclusion filter")
        if _settings.DoTiming then
            this.StartTimer "GetSearchFiles"
        let files = this.GetSearchFiles(startDir)
        if _settings.DoTiming then
            this.StopTimer "GetSearchFiles"
        if _settings.Verbose then
            printfn "Files to be searched:"
            Seq.iter (fun f -> printfn "%s " (f:FileInfo).FullName) files
        if _settings.DoTiming then
            this.StartTimer "SearchFiles"
        //Seq.iter (fun f -> this.SearchFile f) files
        for f in files do
            this.SearchFile f
        if _settings.DoTiming then
            this.StopTimer "SearchFiles"

    member this.SearchFile (f : FileInfo) =
        if _fileUtil.IsUnknownFile f then
            printfn "Skipping file of unknown type: %s" f.FullName
        elif _fileUtil.IsSearchableFile f then
            if _settings.DoTiming then
                this.StartTimer f.FullName
            if _fileUtil.IsTextFile f then
                this.SearchTextFile f
            elif _fileUtil.IsBinaryFile f then
                this.SearchBinaryFile f
            if _settings.DoTiming then
                this.StopTimer f.FullName
        elif _settings.Verbose then
            printfn "Skipping unsearchable file: %s" f.FullName

    member this.SearchTextFile (f : FileInfo) =
        if _settings.Verbose then
            printfn "Searching text file %s" f.FullName
        try
            let mutable lineNum = 0
            use sr = new StreamReader (f.FullName)
            while not sr.EndOfStream do
                let line = sr.ReadLine()
                lineNum <- lineNum + 1
                for p in Seq.filter (fun p -> (p:Regex).Match(line).Success) _settings.SearchPatterns do
                    this.AddSearchResult (new SearchResult(p, f, lineNum, line))
        with
        | :? System.IO.IOException as e -> printfn "%s" e.Message

    member this.SearchBinaryFile (f : FileInfo) =
        if _settings.Verbose then
            printfn "Searching binary file %s" f.FullName
        try
            use sr = new StreamReader (f.FullName)
            let contents = sr.ReadToEnd()
            for p in Seq.filter (fun p -> (p:Regex).Match(contents).Success) _settings.SearchPatterns do
                this.AddSearchResult (new SearchResult(p, f, 0, null))
        with
        | :? System.IO.IOException as ex -> printfn "%s" (ex.Message)

    member this.AddSearchResult (searchResult : SearchResult) =
        printfn "%s" (searchResult.ToString())
        let fileadd = _fileSet.Add(searchResult.File)
        _results.Add(searchResult)
    ;;
