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
    let mutable _archivesOnly = false
    let mutable _debug = false
    let mutable _doTiming = false
    let mutable _excludeHidden = true
    let mutable _firstMatch = false
    let mutable _linesAfter = 0
    let mutable _linesBefore = 0
    let mutable _listDirs = false
    let mutable _listFiles = false
    let mutable _listLines = false
    let mutable _maxLineLength = 150
    let mutable _multiLineSearch = false
    let mutable _printResults = false
    let mutable _printUsage = false
    let mutable _printVersion = false
    let mutable _recursive = true
    let mutable _searchArchives = false
    let mutable _startPath = ""
    let mutable _uniqueLines = false
    let mutable _verbose = false

    // read-only member properties
    member this.InArchiveExtensions = _inArchiveExtensions
    member this.InArchiveFilePatterns = _inArchiveFilePatterns
    member this.InDirPatterns = _inDirPatterns
    member this.InExtensions = _inExtensions
    member this.InFilePatterns = _inFilePatterns
    member this.InLinesAfterPatterns = _inLinesAfterPatterns
    member this.InLinesBeforePatterns = _inLinesBeforePatterns
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
    member this.StartPath
        with get () = _startPath
        and set startPath = _startPath <- startPath
    member this.ArchivesOnly
        with get () = _archivesOnly
        and set archivesOnly = _archivesOnly <- archivesOnly
    member this.Debug
        with get () = _debug
        and set debug = _debug <- debug
    member this.DoTiming
        with get () = _doTiming
        and set doTiming = _doTiming <- doTiming
    member this.ExcludeHidden
        with get () = _excludeHidden
        and set excludeHidden = _excludeHidden <- excludeHidden
    member this.FirstMatch
        with get () = _firstMatch
        and set firstMatch = _firstMatch <- firstMatch
    member this.LinesAfter
        with get () = _linesAfter
        and set linesAfter = _linesAfter <- linesAfter
    member this.LinesBefore
        with get () = _linesBefore
        and set linesBefore = _linesBefore <- linesBefore
    member this.ListDirs
        with get () = _listDirs
        and set listDirs = _listDirs <- listDirs
    member this.ListFiles
        with get () = _listFiles
        and set listFiles = _listFiles <- listFiles
    member this.ListLines
        with get () = _listLines
        and set listLines = _listLines <- listLines
    member this.MaxLineLength
        with get () = _maxLineLength
        and set maxLineLength = _maxLineLength <- maxLineLength
    member this.MultiLineSearch
        with get () = _multiLineSearch
        and set multiLineSearch = _multiLineSearch <- multiLineSearch
    member this.PrintResults
        with get () = _printResults
        and set printResults = _printResults <- printResults
    member this.PrintUsage
        with get () = _printUsage
        and set printUsage = _printUsage <- printUsage
    member this.PrintVersion
        with get () = _printVersion
        and set printVersion = _printVersion <- printVersion
    member this.Recursive
        with get () = _recursive
        and set recursiv = _recursive <- recursiv
    member this.SearchArchives
        with get () = _searchArchives
        and set searchArchives = _searchArchives <- searchArchives
    member this.UniqueLines
        with get() = _uniqueLines
        and set uniqueLines = _uniqueLines <- uniqueLines
    member this.Verbose
        with get() = _verbose
        and set verbose = _verbose <- verbose

    // member methods
    member this.AddExtension(set : HashSet<string>, ext : string) =
        let ext =
            if ext.StartsWith(".") then ext
            else "." + ext
        let success = set.Add(ext)
        ()

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
        this.ArchivesOnly = true &&
        this.SearchArchives = true

    member this.SetDebug() =
        this.Debug = true &&
        this.Verbose = true

    override this.ToString() =
        "SearchSettings(" +
        String.Format("ArchivesOnly: {0}", _archivesOnly) +
        String.Format(", Debug: {0}", _debug) +
        String.Format(", DoTiming: {0}", _doTiming) +
        String.Format(", ExcludeHidden: {0}", _excludeHidden) +
        String.Format(", FirstMatch: {0}", _firstMatch) +
        String.Format(", InDirPatterns: {0}",  "[\"" + String.Join("\", \"", _inDirPatterns) + "\"]") +
        String.Format(", InExtensions: {0}",  "[\"" + String.Join("\", \"", _inExtensions) + "\"]") +
        String.Format(", InFilePatterns: {0}",  "[\"" + String.Join("\", \"", _inFilePatterns) + "\"]") +
        String.Format(", LinesAfter: {0}", _linesAfter) +
        String.Format(", LinesBefore: {0}", _linesBefore) +
        String.Format(", ListDirs: {0}", _listDirs) +
        String.Format(", ListFiles: {0}", _listFiles) +
        String.Format(", ListLines: {0}", _listLines) +
        String.Format(", MaxLineLength: {0}", _maxLineLength) +
        String.Format(", MultiLineSearch: {0}", _multiLineSearch) +
        String.Format(", OutDirPatterns: {0}", "[\"" + String.Join("\", \"", _outDirPatterns) + "\"]") +
        String.Format(", OutExtensions: {0}", "[\"" + String.Join("\", \"", _outExtensions) + "\"]") +
        String.Format(", OutFilePatterns: {0}", "[\"" + String.Join("\", \"", _outFilePatterns) + "\"]") +
        String.Format(", PrintResults: {0}", _printResults) +
        String.Format(", PrintUsage: {0}", _printUsage) +
        String.Format(", PrintVersion: {0}", _printVersion) +
        String.Format(", Recursive: {0}", _recursive) +
        String.Format(", SearchArchives: {0}", _searchArchives) +
        String.Format(", SearchPatterns: {0}", "[\"" + String.Join("\", \"", _searchPatterns) + "\"]") +
        String.Format(", StartPath: \"{0}\"", _startPath) +
        String.Format(", Verbose: {0}", _verbose) +
        ")"
    ;;
