namespace FsSearchLib

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open FsFind

type Searcher (settings : SearchSettings) =
    let _finder = Finder(settings)
    let _binaryEncoding = Encoding.GetEncoding("ISO-8859-1")
    let mutable _textFileEncoding = Encoding.GetEncoding("utf-8")

    // read-only member properties
    member this.BinaryEncoding = _binaryEncoding

    // read-write member properties
    member this.TextFileEncoding 
        with get () = _textFileEncoding
        and set enc = _textFileEncoding <- enc

    // member methods
    member this.EncodingFromStringOrError (encName : string) : string option =
        try
            this.TextFileEncoding <- Encoding.GetEncoding(encName)
            None
        with
        | :? ArgumentException ->
            Some $"Invalid encoding: %s{encName}"

    member this.ValidateSettings () : string list =
        match _finder.ValidateSettings() with
        | [] ->
            [
                (if List.isEmpty settings.SearchPatterns then (Some "No search patterns defined") else None);
                (if settings.LinesBefore < 0 then (Some "Invalid linesbefore") else None);
                (if settings.LinesAfter < 0 then (Some "Invalid linesafter") else None);
                (if settings.MaxLineLength < 0 then (Some "Invalid maxlinelength") else None);
                (this.EncodingFromStringOrError settings.TextFileEncoding);
            ]
            |> List.choose id
        | errs -> errs

    member this.MatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.recGetPatternMatches (patterns : Regex list) (s : string) (pMatches : (Regex * Match) list) = 
        match patterns with
        | [] -> pMatches
        | p :: ps ->
            let allMatches = [for m in p.Matches(s) do yield m]
            let nextMatches =
                if allMatches.IsEmpty
                then []
                else
                    if settings.FirstMatch
                    then allMatches |> List.take 1
                    else allMatches
            let nextPMatches = List.map (fun m -> (p, m)) nextMatches
            this.recGetPatternMatches ps s (List.append pMatches nextPMatches)

    member this.GetLineIndices (s : string) : (int * int) list =
        let newLineIndices =
            Array.zip [|0 .. s.Length - 1|] (s.ToCharArray())
            |> Array.filter (fun x -> snd x = '\n')
            |> Array.map (fun x -> fst x)
            |> List.ofArray
        let startLineIndices =
             0 :: List.map (fun i -> i + 1) newLineIndices
             |> Seq.takeWhile (fun i -> i < s.Length)
             |> List.ofSeq
        //Common.Log (sprintf "startLineIndices (%d): [%s]" startLineIndices.Length (String.Join(",", startLineIndices)))
        let endLineIndices = newLineIndices
        //Common.Log (sprintf "endLineIndices (%d): [%s]" endLineIndices.Length (String.Join(",", endLineIndices)))
        if endLineIndices.Length < startLineIndices.Length then
            List.zip startLineIndices (endLineIndices @ [s.Length - 1])
        else
            List.zip startLineIndices endLineIndices

    member this.LinesMatch (lines : string seq, inPatterns : Regex list,
                            outPatterns : Regex list) : bool =
        (List.isEmpty inPatterns || (this.AnyMatchesAnyPattern lines inPatterns))
        &&
        (List.isEmpty outPatterns || (not (this.AnyMatchesAnyPattern lines outPatterns)))

    member this.LinesBeforeMatch (linesBefore : string seq) : bool =
        this.LinesMatch (linesBefore, settings.InLinesBeforePatterns,
                         settings.OutLinesBeforePatterns)

    member this.LinesAfterMatch (linesAfter : string seq) : bool =
        this.LinesMatch(linesAfter, settings.InLinesAfterPatterns,
                        settings.OutLinesAfterPatterns)

    member this.TakeRight (ss : 'T list) (num : int) : 'T list =
        if ss.Length < num then
            ss
        else
            ss
            |> List.ofSeq
            |> List.rev
            |> Seq.take num
            |> List.ofSeq
            |> List.rev

    member this.SearchContents (s : string) : SearchResult.t list =
        let mutable results : SearchResult.t list = []
        let lineIndices = this.GetLineIndices s
        for p in settings.SearchPatterns do
            let mutable m = p.Match s
            let mutable stop = false
            while m.Success && not stop do
                let matchStartIndex = m.Index
                let matchEndIndex = m.Index + m.Length
                let startLineIndex = Seq.filter (fun i -> fst i < matchStartIndex) lineIndices |> Seq.max |> fst
                //Common.Log (sprintf "startLineIndex: %d" startLineIndex)
                let endLineIndex = Seq.filter (fun i -> snd i > matchStartIndex) lineIndices |> Seq.min |> snd
                //Common.Log (sprintf "endLineIndex: %d" endLineIndex)
                let beforeIndices = Seq.takeWhile (fun i -> fst i < startLineIndex) lineIndices |> List.ofSeq
                //Common.Log (Common.ListToString("beforeIndices", beforeIndices))
                let afterIndices = Seq.skipWhile (fun i -> fst i <= matchStartIndex) lineIndices |> List.ofSeq
                let lineNum = beforeIndices.Length + 1
                let line = s.Substring(startLineIndex, endLineIndex - startLineIndex)
                let beforeLineIndices = this.TakeRight beforeIndices settings.LinesBefore
                //Common.Log (Common.ListToString("beforeLineIndices", beforeLineIndices))
                let linesBefore =
                    if settings.LinesBefore > 0 then
                        beforeLineIndices
                        |> List.map (fun (x,y) -> s.Substring(x, y - x))
                    else
                        []
                let linesAfter =
                    if settings.LinesAfter > 0 then
                        afterIndices
                        |> Seq.take(settings.LinesAfter)
                        |> List.ofSeq
                        |> List.map (fun (x,y) -> s.Substring(x, y - x))
                    else
                        []
                if (linesBefore.Length = 0 || this.LinesBeforeMatch linesBefore) &&
                   (linesAfter.Length = 0 || this.LinesAfterMatch linesAfter) then
                    let r = SearchResult.Create p lineNum (matchStartIndex - startLineIndex + 1) (matchEndIndex - startLineIndex + 1) line linesBefore linesAfter
                    results <- List.append results [r]
                    if settings.FirstMatch then
                        stop <- true
                m <- m.NextMatch()
        results

    member this.SearchTextFileContents (f : FileResult.t) : SearchResult.t list =
        let mutable results : SearchResult.t list = []
        try
            let contents = FileUtil.GetFileContents f.File.FullName this.TextFileEncoding
            results <-
                this.SearchContents contents
                |> List.map (fun r -> { r with File = f })
        with
        | :? IOException as e -> printfn $"%s{e.Message}"
        results

    member this.SearchLine (line : string) (lineNum : int) (linesBefore : string list) (linesAfter : string list) : SearchResult.t list =
        let pMatches = this.recGetPatternMatches settings.SearchPatterns line []
        let toSearchResult (p : Regex) (m : Match) = 
            SearchResult.Create p lineNum (m.Index + 1) (m.Index + m.Length + 1) line linesBefore linesAfter
        let results : SearchResult.t list = List.map (fun (p, m) -> toSearchResult p m) pMatches
        results

    member this.SearchLines (lines : string list) =
        let rec recSearchLines (lineNum : int) (linesBefore : string list) (lines : string list) (results : SearchResult.t list) =
            let nextLines =
                if settings.FirstMatch && (List.length results) = (List.length settings.SearchPatterns)
                then []
                else lines
            match nextLines with
            | [] -> results
            | l :: ls ->
                let linesAfter =
                    if settings.LinesAfter > 0
                    then
                        if (List.length ls < settings.LinesAfter)
                        then ls
                        else (List.take settings.LinesAfter ls)
                    else []
                let rs = this.SearchLine l lineNum linesBefore linesAfter
                let lbs = 
                    if settings.LinesBefore > 0
                    then
                        if (List.length linesBefore) = settings.LinesBefore
                        then (List.append (List.tail linesBefore) [l])
                        else (List.append linesBefore [l])
                    else []
                recSearchLines (lineNum + 1) lbs ls (List.append results rs)
        recSearchLines 1 [] lines []

    member this.SearchTextFileLines (f : FileResult.t) : SearchResult.t list =
        let mutable results : SearchResult.t list = []
        try
            let lines = FileUtil.GetFileLines f.File.FullName this.TextFileEncoding |> List.ofSeq
            results <-
                this.SearchLines lines
                |> List.map (fun r -> { r with File = f })
        with
        | :? IOException as e -> printfn $"%s{e.Message}"
        results

    member this.SearchTextFile (f : FileResult.t) : SearchResult.t list =
        if settings.Debug then
            Common.Log $"Searching text file %s{f.File.FullName}"
        if settings.MultiLineSearch then
            this.SearchTextFileContents f
        else
            this.SearchTextFileLines f

    member this.SearchBlob (blob : string): SearchResult.t list =
        let pMatches = this.recGetPatternMatches settings.SearchPatterns blob []
        let toSearchResult (p : Regex) (m : Match) = 
            SearchResult.Create p 0 (m.Index + 1) (m.Index + m.Length + 1) "" [] []
        let results : SearchResult.t list = List.map (fun (p, m) -> toSearchResult p m) pMatches
        results

    member this.SearchBinaryFile (f : FileResult.t) : SearchResult.t list =
        let mutable results : SearchResult.t list = []
        if settings.Verbose then
            Common.Log $"Searching binary file %s{f.File.FullName}"
        try
            use sr = new StreamReader (f.File.FullName, this.BinaryEncoding)
            let contents = sr.ReadToEnd()
            for p in Seq.filter (fun p -> (p:Regex).Match(contents).Success) settings.SearchPatterns do
                let mutable m = p.Match contents
                let mutable stop = false
                while m.Success && not stop do
                    let matchStartIndex = m.Index + 1
                    let matchEndIndex = m.Index + m.Length + 1
                    let r = SearchResult.Create p 0 matchStartIndex matchEndIndex null [] []
                    results <- List.append results [{ r with File=f }]
                    if settings.FirstMatch then
                        stop <- true
                    m <- m.NextMatch()
        with
        | :? IOException as ex -> printfn $"%s{ex.Message}"
        results

    member this.SearchFile (f : FileResult.t) : SearchResult.t list =
        match f.FileType with
        | FileType.Archive ->
            Common.Log "Archive file searching not currently supported"
            []
        | FileType.Binary -> this.SearchBinaryFile f
        | FileType.Code | FileType.Text | FileType.Xml -> this.SearchTextFile f
        | FileType.Unknown ->
            Common.Log "Skipping file of unknown type"
            []
        | _ ->
            Common.Log $"Skipping file of indeterminate type (this shouldn't happen): %s{f.File.FullName}"
            []

    member this.Search () : SearchResult.t list =
        let files = _finder.Find()
        let mutable results : SearchResult.t list = []

        for file in files do
            let fileSearchResults = this.SearchFile file
            results <- List.append results fileSearchResults
            
        results |> List.ofSeq

    member this.GetSortedResults (results : SearchResult.t list) : SearchResult.t list = 
        results
        |> Seq.sortBy (fun r -> (r.File.File.ToString(), r.LineNum, r.MatchStartIndex, r.MatchEndIndex))
        |> List.ofSeq

    member this.PrintResults (results : SearchResult.t list) : unit =
        let formatter = SearchResultFormatter(settings)
        if results.Length > 0 then
            Common.Log $"\nSearch results (%d{results.Length}):"
            this.GetSortedResults results
            |> Seq.iter (fun r -> Common.Log $"%s{formatter.Format r}")
        else
            Common.Log $"\nSearch results: 0"

    member this.GetMatchingDirs (results : SearchResult.t list) : DirectoryInfo list = 
        results
        |> Seq.map (fun r -> r.File.File.Directory)
        |> Seq.distinctBy (fun d -> d.FullName)
        |> List.ofSeq

    member this.PrintMatchingDirs (results : SearchResult.t list) : unit = 
        let dirs = this.GetMatchingDirs results
        if dirs.Length > 0 then
            Common.Log $"\nMatching directories (%d{dirs.Length}):"
            for d in dirs do
                printfn $"%s{d.FullName}"
        else
            Common.Log "\nMatching directories: 0"

    member this.GetMatchingFiles (results : SearchResult.t list) : FileInfo list = 
        results
        |> Seq.map (fun r -> r.File.File)
        |> List.ofSeq

    member this.PrintMatchingFiles (results : SearchResult.t list) : unit = 
        let files = this.GetMatchingFiles results
        if files.Length > 0 then
            Common.Log $"\nMatching files (%d{files.Length}):"
            for f in files do
                printfn $"%s{f.FullName}"
        else
            Common.Log "\nMatching files: 0"

    member this.GetMatchingLines (results : SearchResult.t list) : string list = 
        let lines =
            results
            |> Seq.map (fun r -> r.Line.Trim())
        if settings.UniqueLines then
            Seq.sortBy (fun (s : string) -> s.ToUpper()) lines
            |> Seq.distinct
            |> List.ofSeq
        else
            Seq.sortBy (fun (s : string) -> s.ToUpper()) lines
            |> List.ofSeq

    member this.PrintMatchingLines (results : SearchResult.t list) : unit = 
        let lines = this.GetMatchingLines results
        let title =
            if settings.UniqueLines then "Unique matching lines"
            else "Matching lines"
        if lines.Length > 0 then
            Common.Log $"\n%s{title} (%d{lines.Length}):"
            for l in lines do
                printfn $"%s{l}"
        else
            Common.Log $"\n%s{title}: 0"
    
;;
