namespace FsSearch

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Searcher (settings : SearchSettings) =
    let _settings = settings
    let _fileTypes = new FileTypes()
    let _results = new List<SearchResult>()
    let _fileSet = new HashSet<FileInfo>()
    let _timers = new Dictionary<string,Stopwatch>()

    // read-only member properties
    member this.Settings = _settings
    member this.Results = _results
    member this.FileSet = _fileSet
    member this.Timers = _timers

    // member methods
    member this.ValidateSettings () : string list =
        let errs = new List<string>()
        if String.IsNullOrEmpty _settings.StartPath then
            errs.Add("Startpath not defined")
        else if not (FileUtil.IsDirectory _settings.StartPath) && not (new FileInfo(_settings.StartPath)).Exists then
            errs.Add("Startpath not found")
        if _settings.SearchPatterns.Count = 0 then errs.Add("No search patterns defined")
        errs |> List.ofSeq

    member this.MatchesAnyPattern (s : string) (patterns : ISet<Regex>) =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : ISet<Regex>) =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.IsSearchDir (d : DirectoryInfo) =
        let elems = d.FullName.Split('/', '\\') |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
        let hiddenOk = not _settings.ExcludeHidden || not (Seq.exists (fun e -> FileUtil.IsHidden e) elems)
        let matchesIn = (Seq.isEmpty _settings.InDirPatterns || this.AnyMatchesAnyPattern elems _settings.InDirPatterns)
        let notMatchesOut = (Seq.isEmpty _settings.OutDirPatterns || not (this.AnyMatchesAnyPattern elems _settings.OutDirPatterns))
        hiddenOk && matchesIn && notMatchesOut

    member this.IsSearchFile (f : FileInfo) =
        (Seq.isEmpty _settings.InExtensions ||
         Seq.exists (fun x -> x = f.Extension) _settings.InExtensions) &&
        (Seq.isEmpty _settings.OutExtensions ||
         not (Seq.exists (fun x -> x = f.Extension) _settings.OutExtensions)) &&
        (Seq.isEmpty _settings.InFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(f.Name).Success) _settings.InFilePatterns) &&
        (Seq.isEmpty _settings.OutFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(f.Name).Success) _settings.OutFilePatterns))

    member this.IsArchiveSearchFile (f : FileInfo) =
        (Seq.isEmpty _settings.InArchiveExtensions ||
         Seq.exists (fun x -> x = f.Extension) _settings.InArchiveExtensions) &&
        (Seq.isEmpty _settings.OutArchiveExtensions ||
         not (Seq.exists (fun x -> x = f.Extension) _settings.OutArchiveExtensions)) &&
        (Seq.isEmpty _settings.InArchiveFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(f.Name).Success) _settings.InArchiveFilePatterns) &&
        (Seq.isEmpty _settings.OutArchiveFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(f.Name).Success) _settings.OutArchiveFilePatterns))

    member this.FilterFile (f: FileInfo) = 
        if FileUtil.IsHidden f.Name && _settings.ExcludeHidden then
            false
        else if _fileTypes.IsArchiveFile f then
            _settings.SearchArchives && this.IsArchiveSearchFile f
        else
            not _settings.ArchivesOnly && this.IsSearchFile f

    member this.StartTimer (name : string) =
        let timer = new Stopwatch()
        timer.Start()
        _timers.Add(name, timer)

    member this.StopTimer (name : string) =
        let timer = _timers.[name]
        timer.Stop()
        Common.PrintElapsed name timer.Elapsed

    member this.GetSearchDirs (dir : DirectoryInfo) : DirectoryInfo list =
        try
            let dirs = 
                dir.EnumerateDirectories()
                |> Seq.filter (fun d -> this.IsSearchDir (d:DirectoryInfo))
                |> List.ofSeq
            dirs @ List.fold (fun ds d -> (ds @ (this.GetSearchDirs d))) [] dirs
        with
        | :? IOException as e ->
            Common.Log (sprintf "Error while accessing dir %s: %s" dir.FullName e.Message)
            List.empty<DirectoryInfo>

    member this.GetSearchFilesForDir (dir : DirectoryInfo) =
        try
            dir.EnumerateFiles()
            |> Seq.filter (fun f -> this.FilterFile (f:FileInfo))
            |> List.ofSeq
        with
        | :? IOException as e ->
            Common.Log (sprintf "Error while accessing dir %s: %s" dir.FullName e.Message)
            List.empty<FileInfo>

    member this.GetSearchFiles (dirs : DirectoryInfo list) =
        let rec getSearchFiles (files : FileInfo list, dirs : DirectoryInfo list) =
            match dirs with
            | [] -> files
            | d :: ds -> getSearchFiles(files @ (this.GetSearchFilesForDir d), ds)
        getSearchFiles([], dirs)

    member this.Search () =
        if FileUtil.IsDirectory(_settings.StartPath) then
            let startDir = new DirectoryInfo(_settings.StartPath)
            if this.IsSearchDir startDir then
                this.SearchPath startDir
            else
                raise <| Exception ("Startpath does not match search settings")
        else
            this.SearchFile (new FileInfo(_settings.StartPath))

    member this.SearchPath (startDir : DirectoryInfo) =
        if _settings.DoTiming then
            this.StartTimer "GetSearchDirs"
        let dirs =
            if _settings.Recursive then
                startDir :: this.GetSearchDirs(startDir)
            else
                [startDir]
        if _settings.DoTiming then
            this.StopTimer "GetSearchDirs"
        if _settings.Verbose then
            printfn "\nDirectories to be searched (%d):" dirs.Length
            List.iter (fun d -> Common.Log (sprintf "%s " (d:DirectoryInfo).FullName)) dirs
        
        if _settings.DoTiming then
            this.StartTimer "GetSearchFiles"
        let files = this.GetSearchFiles(dirs)
        if _settings.DoTiming then
            this.StopTimer "GetSearchFiles"
        if _settings.Verbose then
            Common.Log (sprintf "\nFiles to be searched (%d):" files.Length)
            Seq.iter (fun f -> Common.Log (sprintf "%s " (f:FileInfo).FullName)) files
            printfn ""

        if _settings.DoTiming then
            this.StartTimer "SearchFiles"
        for f in files do
            this.SearchFile f
        if _settings.DoTiming then
            this.StopTimer "SearchFiles"

    member this.SearchFile (f : FileInfo) =
        if _settings.DoTiming then
            this.StartTimer f.FullName
        match _fileTypes.GetFileType f with
        | FileType.Archive -> Common.Log (sprintf "Archive file searching not currently supported")
        | FileType.Binary -> this.SearchBinaryFile f
        | FileType.Text -> this.SearchTextFile f
        | FileType.Unknown -> Common.Log (sprintf "Skipping file of unknown type")
        | _ -> ()
        if _settings.DoTiming then
            this.StopTimer f.FullName

    member this.SearchTextFile (f : FileInfo) =
        if _settings.Verbose then
            Common.Log (sprintf "Searching text file %s" f.FullName)
        if _settings.MultiLineSearch then
            this.SearchTextFileContents f
        else
            this.SearchTextFileLines f

    member this.SearchTextFileContents (f : FileInfo) =
        try
            let contents = FileUtil.GetFileContents f.FullName
            let results = this.SearchContents(contents)
            for r:SearchResult in results do
                r.File <- f
                this.AddSearchResult r
        with
        | :? IOException as e -> printfn "%s" e.Message

    member this.GetLineIndices (s : string) =
        let newLineIndices =
            Array.zip [|0 .. s.Length - 1|] (s.ToCharArray())
            |> Array.filter (fun x -> snd x = '\n')
            |> Array.map (fun x -> fst x)
            |> List.ofArray
        let startLineIndices =
             0 :: List.map (fun i -> i + 1) newLineIndices
             |> Seq.takeWhile (fun i -> i < s.Length)
             |> List.ofSeq
        let endLineIndices = newLineIndices
        List.zip startLineIndices endLineIndices

    member this.SearchContents (s : string) =
        let patternMatches = new Dictionary<Regex, int>()
        let results = new List<SearchResult>()
        let lineIndices = this.GetLineIndices s
        //Common.Log (Common.ListToString("lineIndices", lineIndices))
        for p in _settings.SearchPatterns do
            let mutable m = p.Match s
            let mutable stop = false
            while m.Success && not stop do
                let matchStartIndex = m.Index
                //Common.Log (sprintf "matchStartIndex: %d" matchStartIndex)
                let matchEndIndex = m.Index + m.Length
                //Common.Log (sprintf "matchEndIndex: %d" matchEndIndex)
                let beforeIndices = Seq.takeWhile (fun i -> fst i <= matchStartIndex) lineIndices |> List.ofSeq
                //Common.Log (Common.ListToString("beforeIndices", beforeIndices))
                let afterIndices = Seq.skipWhile (fun i -> fst i <= matchStartIndex) lineIndices |> List.ofSeq
                //Common.Log (Common.ListToString("afterIndices", afterIndices))
                let lineNum = beforeIndices.Length
                //Common.Log (sprintf "lineNum: %d" lineNum)
                let startLineIndex = Seq.last beforeIndices |> fst
                //Common.Log (sprintf "startLineIndex: %d" startLineIndex)
                let endLineIndex = Seq.last beforeIndices |> snd
                //Common.Log (sprintf "endLineIndex: %d" endLineIndex)
                let line = s.Substring(startLineIndex, endLineIndex - startLineIndex)
                results.Add(new SearchResult(p,
                                             null,
                                             lineNum,
                                             matchStartIndex - startLineIndex + 1,
                                             matchEndIndex - startLineIndex + 1,
                                             line,
                                             [],
                                             []))
                if _settings.FirstMatch then
                    stop <- true
                m <- m.NextMatch()
        results

    member this.SearchTextFileLines (f : FileInfo) =
        try
            let lines = FileUtil.GetFileLines f.FullName
            let results = this.SearchLines lines
            for r:SearchResult in results do
                r.File <- f
                this.AddSearchResult r
        with
        | :? IOException as e -> printfn "%s" e.Message

    member this.SearchLines (lines : string seq) =
        let patternMatches = new Dictionary<Regex, int>()
        let results = new List<SearchResult>()
        let mutable lineNum = 0
        for line in lines do
            lineNum <- lineNum + 1
            for p in _settings.SearchPatterns do
                if not _settings.FirstMatch || not (patternMatches.ContainsKey p) then
                    let matches = p.Matches(line)
                    if matches.Count > 0 then
                        if not (patternMatches.ContainsKey p) then
                            patternMatches.Add(p, 1)
                        if _settings.FirstMatch then
                            let m = matches.[0]
                            let matchStartIndex = m.Index + 1
                            let matchEndIndex = m.Index + m.Length + 1
                            results.Add(new SearchResult(p, null, lineNum, matchStartIndex, matchEndIndex, line, [], []))
                        else
                            for m in matches do
                                let matchStartIndex = m.Index + 1
                                let matchEndIndex = m.Index + m.Length + 1
                                results.Add(new SearchResult(p, null, lineNum, matchStartIndex, matchEndIndex, line, [], []))
        results

    member this.SearchBinaryFile (f : FileInfo) =
        if _settings.Verbose then
            Common.Log (sprintf "Searching binary file %s" f.FullName)
        try
            use sr = new StreamReader (f.FullName)
            let contents = sr.ReadToEnd()
            for p in Seq.filter (fun p -> (p:Regex).Match(contents).Success) _settings.SearchPatterns do
                this.AddSearchResult (new SearchResult(p, f, 0, 0, 0, null, [], []))
        with
        | :? IOException as ex -> printfn "%s" (ex.Message)

    member this.AddSearchResult (searchResult : SearchResult) =
        let fileadd = _fileSet.Add(searchResult.File)
        _results.Add(searchResult)

    member this.GetMatchingDirs : DirectoryInfo list = 
        this.Results
        |> Seq.map (fun r -> r.File.Directory)
        |> Seq.distinctBy (fun d -> d.FullName)
        |> Seq.sortBy (fun d -> d.FullName)
        |> List.ofSeq

    member this.PrintMatchingDirs = 
        let dirs = this.GetMatchingDirs
        Common.Log (sprintf "\nDirectories with matches (%d):" dirs.Length)
        for d in dirs do
            printfn "%s" d.FullName

    member this.GetMatchingFiles : FileInfo list = 
        this.Results
        |> Seq.map (fun r -> r.File)
        |> Seq.distinctBy (fun f -> f.FullName)
        |> Seq.sortBy (fun f -> f.FullName)
        |> List.ofSeq

    member this.PrintMatchingFiles = 
        let files = this.GetMatchingFiles
        Common.Log (sprintf "\nFiles with matches (%d):" files.Length)
        for f in files do
            printfn "%s" f.FullName

    member this.GetMatchingLines : string list = 
        let lines =
            this.Results
            |> Seq.map (fun r -> r.Line.Trim())
        if _settings.UniqueLines then
            lines
            |> Seq.distinct
            |> Seq.sort
            |> List.ofSeq
        else
            lines
            |> Seq.sort
            |> List.ofSeq

    member this.PrintMatchingLines = 
        let lines = this.GetMatchingLines
        let title =
            if _settings.UniqueLines then "Unique lines with matches"
            else "Lines with matches"
        Common.Log (sprintf "\n%s (%d):" title lines.Length)
        for l in lines do
            printfn "%s" l


    ;;
