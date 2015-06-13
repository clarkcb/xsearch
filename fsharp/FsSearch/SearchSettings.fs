namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type SearchSettings() =
    let _inArchiveExtensions = new HashSet<string>()
    let _inArchiveFilePatterns = new HashSet<Regex>()
    let _inDirPatterns = new HashSet<Regex>()
    let _inExtensions = new HashSet<string>()
    let _inFilePatterns = new HashSet<Regex>()
    let _inLinesAfterPatterns = new HashSet<Regex>()
    let _inLinesBeforePatterns = new HashSet<Regex>()
    let _linesAfterToPatterns = new HashSet<Regex>()
    let _linesAfterUntilPatterns = new HashSet<Regex>()
    let _outArchiveExtensions = new HashSet<string>()
    let _outArchiveFilePatterns = new HashSet<Regex>()
    let _outDirPatterns = new HashSet<Regex>()
    let _outExtensions = new HashSet<string>()
    let _outFilePatterns = new HashSet<Regex>()
    let _outLinesAfterPatterns = new HashSet<Regex>()
    let _outLinesBeforePatterns = new HashSet<Regex>()
    let _searchPatterns = new HashSet<Regex>()

    // read-only member properties
    member this.InArchiveExtensions = _inArchiveExtensions
    member this.InArchiveFilePatterns = _inArchiveFilePatterns
    member this.InDirPatterns = _inDirPatterns
    member this.InExtensions = _inExtensions
    member this.InFilePatterns = _inFilePatterns
    member this.InLinesAfterPatterns = _inLinesAfterPatterns
    member this.InLinesBeforePatterns = _inLinesBeforePatterns
    member this.LinesAfterToPatterns = _linesAfterToPatterns
    member this.LinesAfterUntilPatterns = _linesAfterUntilPatterns
    member this.OutArchiveExtensions = _outArchiveExtensions
    member this.OutArchiveFilePatterns = _outArchiveFilePatterns
    member this.OutDirPatterns = _outDirPatterns
    member this.OutExtensions = _outExtensions
    member this.OutFilePatterns = _outFilePatterns
    member this.OutLinesAfterPatterns = _outLinesAfterPatterns
    member this.OutLinesBeforePatterns = _outLinesBeforePatterns
    member this.SearchPatterns = _searchPatterns

    member this.HasExtensions = Seq.length _inExtensions > 0 || Seq.length _outExtensions > 0
    member this.HasDirPatterns = Seq.length _inDirPatterns > 0 || Seq.length _outDirPatterns > 0
    member this.HasFilePatterns = Seq.length _inFilePatterns > 0 || Seq.length _outFilePatterns > 0

    // read-write member properties
    member val StartPath = "" with get,set
    member val ArchivesOnly = false with get,set
    member val Debug = false with get,set
    member val DoTiming = false with get,set
    member val ExcludeHidden = true with get,set
    member val FirstMatch = false with get,set
    member val LinesAfter = 0 with get,set
    member val LinesBefore = 0 with get,set
    member val ListDirs = false with get,set
    member val ListFiles = false with get,set
    member val ListLines = false with get,set
    member val MaxLineLength = 150 with get,set
    member val MultiLineSearch = false with get,set
    member val PrintResults = false with get,set
    member val PrintUsage = false with get,set
    member val PrintVersion = false with get,set
    member val Recursive = true with get,set
    member val SearchArchives = false with get,set
    member val UniqueLines = false with get,set
    member val Verbose = false with get,set

    // member methods
    member this.AddExtension(set : HashSet<string>, exts : string) =
        let xs = exts.Split(',') |> Seq.filter (fun x -> not (String.IsNullOrEmpty x))
        for x in xs do
            let ext =
                if x.StartsWith(".") then x
                else "." + x
            set.Add(ext) |> ignore

    member this.AddPattern(set : HashSet<Regex>, pattern : string) =
        let success = set.Add(new Regex(pattern))
        ()

    member this.AddInArchiveExtension(ext : string) =
        this.AddExtension(_inArchiveExtensions, ext)

    member this.AddInArchiveFilePattern(pattern : string) =
        this.AddPattern(_inArchiveFilePatterns, pattern)

    member this.AddInDirPattern(pattern : string) =
        this.AddPattern(_inDirPatterns, pattern)

    member this.AddInExtension(ext : string) =
        this.AddExtension(_inExtensions, ext)

    member this.AddInFilePattern(pattern : string) =
        this.AddPattern(_inFilePatterns, pattern)

    member this.AddInLinesAfterPattern(pattern : string) =
        this.AddPattern(_inLinesAfterPatterns, pattern)

    member this.AddInLinesBeforePattern(pattern : string) =
        this.AddPattern(_inLinesBeforePatterns, pattern)

    member this.AddLinesAfterToPattern(pattern : string) =
        this.AddPattern(_linesAfterToPatterns, pattern)

    member this.AddLinesAfterUntilPattern(pattern : string) =
        this.AddPattern(_linesAfterUntilPatterns, pattern)

    member this.AddOutArchiveExtension(ext : string) =
        this.AddExtension(_outArchiveExtensions, ext)

    member this.AddOutArchiveFilePattern(pattern : string) =
        this.AddPattern(_outArchiveFilePatterns, pattern)

    member this.AddOutDirPattern(pattern : string) =
        this.AddPattern(_outDirPatterns, pattern)

    member this.AddOutExtension(ext : string) =
        this.AddExtension(_outExtensions, ext)

    member this.AddOutFilePattern(pattern : string) =
        this.AddPattern(_outFilePatterns, pattern)

    member this.AddOutLinesAfterPattern(pattern : string) =
        this.AddPattern(_outLinesAfterPatterns, pattern)

    member this.AddOutLinesBeforePattern(pattern : string) =
        this.AddPattern(_outLinesBeforePatterns, pattern)

    member this.AddSearchPattern(pattern : string) =
        this.AddPattern(_searchPatterns, pattern)

    member this.SetArchivesOnly() =
        this.ArchivesOnly <- true
        this.SearchArchives <- true

    member this.SetDebug() =
        this.Debug <- true
        this.Verbose <- true

    override this.ToString() =
        "SearchSettings(" +
        String.Format("ArchivesOnly: {0}", this.ArchivesOnly) +
        String.Format(", Debug: {0}", this.Debug) +
        String.Format(", DoTiming: {0}", this.DoTiming) +
        String.Format(", ExcludeHidden: {0}", this.ExcludeHidden) +
        String.Format(", FirstMatch: {0}", this.FirstMatch) +
        String.Format(", InArchiveExtensions: {0}",  "[\"" + String.Join("\", \"", _inArchiveExtensions) + "\"]") +
        String.Format(", InArchiveFilePatterns: {0}",  "[\"" + String.Join("\", \"", _inArchiveFilePatterns) + "\"]") +
        String.Format(", InDirPatterns: {0}",  "[\"" + String.Join("\", \"", _inDirPatterns) + "\"]") +
        String.Format(", InExtensions: {0}",  "[\"" + String.Join("\", \"", _inExtensions) + "\"]") +
        String.Format(", InFilePatterns: {0}",  "[\"" + String.Join("\", \"", _inFilePatterns) + "\"]") +
        String.Format(", InLinesAfterPatterns: {0}",  "[\"" + String.Join("\", \"", _inLinesAfterPatterns) + "\"]") +
        String.Format(", InLinesBeforePatterns: {0}",  "[\"" + String.Join("\", \"", _inLinesBeforePatterns) + "\"]") +
        String.Format(", LinesAfter: {0}", this.LinesAfter) +
        String.Format(", LinesAfterToPatterns: {0}",  "[\"" + String.Join("\", \"", _linesAfterToPatterns) + "\"]") +
        String.Format(", LinesAfterUntilPatterns: {0}",  "[\"" + String.Join("\", \"", _linesAfterUntilPatterns) + "\"]") +
        String.Format(", LinesBefore: {0}", this.LinesBefore) +
        String.Format(", ListDirs: {0}", this.ListDirs) +
        String.Format(", ListFiles: {0}", this.ListFiles) +
        String.Format(", ListLines: {0}", this.ListLines) +
        String.Format(", MaxLineLength: {0}", this.MaxLineLength) +
        String.Format(", MultiLineSearch: {0}", this.MultiLineSearch) +
        String.Format(", OutArchiveExtensions: {0}",  "[\"" + String.Join("\", \"", _outArchiveExtensions) + "\"]") +
        String.Format(", OutArchiveFilePatterns: {0}",  "[\"" + String.Join("\", \"", _outArchiveFilePatterns) + "\"]") +
        String.Format(", OutDirPatterns: {0}", "[\"" + String.Join("\", \"", _outDirPatterns) + "\"]") +
        String.Format(", OutExtensions: {0}", "[\"" + String.Join("\", \"", _outExtensions) + "\"]") +
        String.Format(", OutFilePatterns: {0}", "[\"" + String.Join("\", \"", _outFilePatterns) + "\"]") +
        String.Format(", OutLinesAfterPatterns: {0}",  "[\"" + String.Join("\", \"", _outLinesAfterPatterns) + "\"]") +
        String.Format(", OutLinesBeforePatterns: {0}",  "[\"" + String.Join("\", \"", _outLinesBeforePatterns) + "\"]") +
        String.Format(", PrintResults: {0}", this.PrintResults) +
        String.Format(", PrintUsage: {0}", this.PrintUsage) +
        String.Format(", PrintVersion: {0}", this.PrintVersion) +
        String.Format(", Recursive: {0}", this.Recursive) +
        String.Format(", SearchArchives: {0}", this.SearchArchives) +
        String.Format(", SearchPatterns: {0}", "[\"" + String.Join("\", \"", _searchPatterns) + "\"]") +
        String.Format(", StartPath: \"{0}\"", this.StartPath) +
        String.Format(", UniqueLines: {0}", this.UniqueLines) +
        String.Format(", Verbose: {0}", this.Verbose) +
        ")"
    ;;
