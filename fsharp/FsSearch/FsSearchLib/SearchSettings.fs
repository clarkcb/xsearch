namespace FsSearchLib

open System.Text.RegularExpressions

module SearchSettings =
    type t = {
        ArchivesOnly : bool;
        Colorize : bool;
        Debug : bool;
        ExcludeHidden : bool;
        FirstMatch : bool;
        InArchiveExtensions : string list;
        InArchiveFilePatterns : Regex list;
        InDirPatterns : Regex list;
        InExtensions : string list;
        InFilePatterns : Regex list;
        InFileTypes : FileType list;
        InLinesAfterPatterns : Regex list;
        InLinesBeforePatterns : Regex list;
        LinesAfter : int;
        LinesAfterToPatterns : Regex list;
        LinesAfterUntilPatterns : Regex list;
        LinesBefore : int;
        ListDirs : bool;
        ListFiles : bool;
        ListLines : bool;
        MaxLineLength : int;
        MultiLineSearch : bool;
        OutArchiveExtensions : string list;
        OutArchiveFilePatterns : Regex list;
        OutDirPatterns : Regex list;
        OutExtensions : string list;
        OutFilePatterns : Regex list;
        OutFileTypes : FileType list;
        OutLinesAfterPatterns : Regex list;
        OutLinesBeforePatterns : Regex list;
        Paths : string list;
        PrintResults : bool;
        PrintUsage : bool;
        PrintVersion : bool;
        Recursive : bool;
        SearchArchives : bool;
        SearchPatterns : Regex list;
        TextFileEncoding : string;
        UniqueLines : bool;
        Verbose : bool
    }

    let DefaultSettings = {
        ArchivesOnly = false;
        Colorize = true;
        Debug = false;
        ExcludeHidden = true;
        FirstMatch = false;
        InArchiveExtensions = [];
        InArchiveFilePatterns = [];
        InDirPatterns = [];
        InExtensions = [];
        InFilePatterns = [];
        InFileTypes = [];
        InLinesAfterPatterns = [];
        InLinesBeforePatterns = [];
        LinesAfter = 0;
        LinesAfterToPatterns = [];
        LinesAfterUntilPatterns = [];
        LinesBefore = 0;
        ListDirs = false;
        ListFiles = false;
        ListLines = false;
        MaxLineLength = 150;
        MultiLineSearch = false;
        OutArchiveExtensions = [];
        OutArchiveFilePatterns = [];
        OutDirPatterns = [];
        OutExtensions = [];
        OutFilePatterns = [];
        OutFileTypes = [];
        OutLinesAfterPatterns = [];
        OutLinesBeforePatterns = [];
        Paths = [];
        PrintResults = false;
        PrintUsage = false;
        PrintVersion = false;
        Recursive = true;
        SearchArchives = false;
        SearchPatterns = [];
        TextFileEncoding = "utf-8";
        UniqueLines = false;
        Verbose = false
    }

    let AddExtensions (exts : string) (extList : string list) : string list =
        List.append extList (FileUtil.ExtensionsListFromString exts)

    let AddPattern (pattern : string) (patternList : Regex list) : Regex list =
        List.append patternList [Regex(pattern)]

    let SetArchivesOnly (archivesOnly : bool) (settings : t) : t =
        match archivesOnly with
        | true -> { settings with ArchivesOnly=true; SearchArchives=true }
        | _ -> { settings with ArchivesOnly=false }

    let SetDebug (debug : bool) (settings : t) : t =
        match debug with
        | true -> { settings with Debug=true; Verbose=true }
        | _ -> { settings with Debug=false }

    let FileTypesListToString (lst : FileType list) : string = 
        let rec recListToString (acc : string) (lst : FileType list) =
            match lst with
            | []     -> acc.Trim()
            | [a]    -> (recListToString (acc + " \"" + (FileTypes.ToName a) + "\"") [])
            | h :: t -> (recListToString (acc + " \"" + (FileTypes.ToName h) + "\";") t) in
        sprintf "[%s]" (recListToString "" lst)

    let ToString settings =
        String.concat "" [
            "SearchSettings(";
            $"ArchivesOnly: %b{settings.ArchivesOnly}";
            $", Colorize: %b{settings.Colorize}";
            $", Debug: %b{settings.Debug}";
            $", ExcludeHidden: %b{settings.ExcludeHidden}";
            $", FirstMatch: %b{settings.FirstMatch}";
            $", InArchiveExtensions: %s{Common.list_to_string(settings.InArchiveExtensions)}";
            $", InArchiveFilePatterns: %s{Common.list_to_string(settings.InArchiveFilePatterns)}";
            $", InDirPatterns: %s{Common.list_to_string(settings.InDirPatterns)}";
            $", InExtensions: %s{Common.list_to_string(settings.InExtensions)}";
            $", InFilePatterns: %s{Common.list_to_string(settings.InFilePatterns)}";
            $", InFileTypes: %s{FileTypesListToString settings.InFileTypes}";
            $", InLinesAfterPatterns: %s{Common.list_to_string(settings.InLinesAfterPatterns)}";
            $", InLinesBeforePatterns: %s{Common.list_to_string(settings.InLinesBeforePatterns)}";
            $", LinesAfter: %d{settings.LinesAfter}";
            $", LinesAfterToPatterns: %s{Common.list_to_string(settings.LinesAfterToPatterns)}";
            $", LinesAfterUntilPatterns: %s{Common.list_to_string(settings.LinesAfterUntilPatterns)}";
            $", LinesBefore: %d{settings.LinesBefore}";
            $", ListDirs: %b{settings.ListDirs}";
            $", ListFiles: %b{settings.ListFiles}";
            $", ListLines: %b{settings.ListLines}";
            $", MaxLineLength: %d{settings.MaxLineLength}";
            $", MultiLineSearch: %b{settings.MultiLineSearch}";
            $", OutArchiveExtensions: %s{Common.list_to_string(settings.OutArchiveExtensions)}";
            $", OutArchiveFilePatterns: %s{Common.list_to_string(settings.OutArchiveFilePatterns)}";
            $", OutDirPatterns: %s{Common.list_to_string(settings.OutDirPatterns)}";
            $", OutExtensions: %s{Common.list_to_string(settings.OutExtensions)}";
            $", OutFilePatterns: %s{Common.list_to_string(settings.OutFilePatterns)}";
            $", OutFileTypes: %s{FileTypesListToString settings.OutFileTypes}";
            $", OutLinesAfterPatterns: %s{Common.list_to_string(settings.OutLinesAfterPatterns)}";
            $", OutLinesBeforePatterns: %s{Common.list_to_string(settings.OutLinesBeforePatterns)}";
            $", Paths: %s{Common.list_to_string(settings.Paths)}";
            $", PrintResults: %b{settings.PrintResults}";
            $", PrintUsage: %b{settings.PrintUsage}";
            $", PrintVersion: %b{settings.PrintVersion}";
            $", Recursive: %b{settings.Recursive}";
            $", SearchArchives: %b{settings.SearchArchives}";
            $", SearchPatterns: %s{Common.list_to_string(settings.SearchPatterns)}";
            $", TextFileEncoding: \"%s{settings.TextFileEncoding}\"";
            $", UniqueLines: %b{settings.UniqueLines}";
            $", Verbose: %b{settings.Verbose}";
            ")"
        ]
;;
