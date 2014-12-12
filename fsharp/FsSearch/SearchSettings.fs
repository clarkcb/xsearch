namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type SearchSettings() =
    let _inExtensions = new HashSet<string>()
    let _outExtensions = new HashSet<string>()

    let _inDirPatterns = new HashSet<Regex>()
    let _outDirPatterns = new HashSet<Regex>()
    let _inFilePatterns = new HashSet<Regex>()
    let _outFilePatterns = new HashSet<Regex>()
    let _searchPatterns = new HashSet<Regex>()

    let mutable _startPath = ""

    let mutable _archivesOnly = false
    let mutable _debug = false
    let mutable _doTiming = false
    let mutable _firstMatch = false
    let mutable _listDirs = false
    let mutable _listFiles = false
    let mutable _listLines = false
    let mutable _printResults = false
    let mutable _printUsage = false
    let mutable _printVersion = false
    let mutable _recursive = true
    let mutable _searchArchives = false
    let mutable _uniqueLines = false
    let mutable _verbose = false

    // read-only member properties
    member this.InExtensions = _inExtensions
    member this.OutExtensions = _outExtensions
    member this.InDirPatterns = _inDirPatterns
    member this.OutDirPatterns = _outDirPatterns
    member this.InFilePatterns = _inFilePatterns
    member this.OutFilePatterns = _outFilePatterns
    member this.SearchPatterns = _searchPatterns

    member this.HasExtensions = _inExtensions.Count > 0 || _outExtensions.Count > 0
    member this.HasDirPatterns = _inDirPatterns.Count > 0 || _outDirPatterns.Count > 0
    member this.HasFilePatterns = _inFilePatterns.Count > 0 || _outFilePatterns.Count > 0


    // read-write member properties
    member this.StartPath
        with get () = _startPath
        and set startPath = _startPath <- startPath
    member this.ArchivesOnly
        with get() = _archivesOnly
        and set archivesOnly = _archivesOnly <- archivesOnly
    member this.Debug
        with get() = _debug
        and set debug = _debug <- debug
    member this.DoTiming
        with get() = _doTiming
        and set doTiming = _doTiming <- doTiming
    member this.FirstMatch
        with get() = _firstMatch
        and set firstMatch = _firstMatch <- firstMatch
    member this.ListDirs
        with get () = _listDirs
        and set listDirs = _listDirs <- listDirs
    member this.ListFiles
        with get () = _listFiles
        and set listFiles = _listFiles <- listFiles
    member this.ListLines
        with get () = _listLines
        and set listLines = _listLines <- listLines
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

    member this.AddInExtension(ext : string) =
        this.AddExtension(_inExtensions, ext)

    member this.AddOutExtension(ext : string) =
        this.AddExtension(_outExtensions, ext)

    member this.AddPattern(set : HashSet<Regex>, pattern : string) =
        let success = set.Add(new Regex(pattern))
        ()

    member this.AddInDirPattern(pattern : string) =
        this.AddPattern(_inDirPatterns, pattern)

    member this.AddOutDirPattern(pattern : string) =
        this.AddPattern(_outDirPatterns, pattern)

    member this.AddInFilePattern(pattern : string) =
        this.AddPattern(_inFilePatterns, pattern)

    member this.AddOutFilePattern(pattern : string) =
        this.AddPattern(_outFilePatterns, pattern)

    member this.AddSearchPattern(pattern : string) =
        this.AddPattern(_searchPatterns, pattern)

    override this.ToString() =
        "SearchSettings(" +
        String.Format("StartPath: \"{0}\"", _startPath) +
        String.Format(", InExtensions: {0}",  "[\"" + String.Join("\", \"", _inExtensions) + "\"]") +
        String.Format(", OutExtensions: {0}", "[\"" + String.Join("\", \"", _outExtensions) + "\"]") +
        String.Format(", InDirPatterns: {0}",  "[\"" + String.Join("\", \"", _inDirPatterns) + "\"]") +
        String.Format(", OutDirPatterns: {0}", "[\"" + String.Join("\", \"", _outDirPatterns) + "\"]") +
        String.Format(", InFilePatterns: {0}",  "[\"" + String.Join("\", \"", _inFilePatterns) + "\"]") +
        String.Format(", OutFilePatterns: {0}", "[\"" + String.Join("\", \"", _outFilePatterns) + "\"]") +
        String.Format(", SearchPatterns: {0}", "[\"" + String.Join("\", \"", _searchPatterns) + "\"]") +
        String.Format(", ArchivesOnly: {0}", _archivesOnly) +
        String.Format(", Debug: {0}", _debug) +
        String.Format(", DoTiming: {0}", _doTiming) +
        String.Format(", FirstMatch: {0}", _firstMatch) +
        String.Format(", ListDirs: {0}", _listDirs) +
        String.Format(", ListFiles: {0}", _listFiles) +
        String.Format(", ListLines: {0}", _listLines) +
        String.Format(", PrintResults: {0}", _printResults) +
        String.Format(", PrintUsage: {0}", _printUsage) +
        String.Format(", PrintVersion: {0}", _printVersion) +
        String.Format(", Recursive: {0}", _recursive) +
        String.Format(", SearchArchives: {0}", _searchArchives) +
        String.Format(", Verbose: {0}", _verbose) +
        ")"
    ;;
