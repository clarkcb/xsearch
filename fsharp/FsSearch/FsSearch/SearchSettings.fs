namespace FsSearch

open System.Text.RegularExpressions

module SearchSettings =
    type t = {
        ArchivesOnly : bool;
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
        PrintResults : bool;
        PrintUsage : bool;
        PrintVersion : bool;
        Recursive : bool;
        SearchArchives : bool;
        SearchPatterns : Regex list;
        StartPath : string;
        TextFileEncoding : string;
        UniqueLines : bool;
        Verbose : bool
    }

    let DefaultSettings = {
        ArchivesOnly = false;
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
        PrintResults = false;
        PrintUsage = false;
        PrintVersion = false;
        Recursive = true;
        SearchArchives = false;
        SearchPatterns = [];
        StartPath = "";
        TextFileEncoding = "utf-8";
        UniqueLines = false;
        Verbose = false
    }

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
            sprintf "ArchivesOnly: %b" settings.ArchivesOnly;
            sprintf ", Debug: %b" settings.Debug;
            sprintf ", ExcludeHidden: %b" settings.ExcludeHidden;
            sprintf ", FirstMatch: %b" settings.FirstMatch;
            sprintf ", InArchiveExtensions: %s" (Common.list_to_string(settings.InArchiveExtensions));
            sprintf ", InArchiveFilePatterns: %s" (Common.list_to_string(settings.InArchiveFilePatterns));
            sprintf ", InDirPatterns: %s" (Common.list_to_string(settings.InDirPatterns));
            sprintf ", InExtensions: %s" (Common.list_to_string(settings.InExtensions));
            sprintf ", InFilePatterns: %s" (Common.list_to_string(settings.InFilePatterns));
            sprintf ", InFileTypes: %s" (FileTypesListToString settings.InFileTypes);
            sprintf ", InLinesAfterPatterns: %s" (Common.list_to_string(settings.InLinesAfterPatterns));
            sprintf ", InLinesBeforePatterns: %s" (Common.list_to_string(settings.InLinesBeforePatterns));
            sprintf ", LinesAfter: %d" settings.LinesAfter;
            sprintf ", LinesAfterToPatterns: %s" (Common.list_to_string(settings.LinesAfterToPatterns));
            sprintf ", LinesAfterUntilPatterns: %s" (Common.list_to_string(settings.LinesAfterUntilPatterns));
            sprintf ", LinesBefore: %d" settings.LinesBefore;
            sprintf ", ListDirs: %b" settings.ListDirs;
            sprintf ", ListFiles: %b" settings.ListFiles;
            sprintf ", ListLines: %b" settings.ListLines;
            sprintf ", MaxLineLength: %d" settings.MaxLineLength;
            sprintf ", MultiLineSearch: %b" settings.MultiLineSearch;
            sprintf ", OutArchiveExtensions: %s" (Common.list_to_string(settings.OutArchiveExtensions));
            sprintf ", OutArchiveFilePatterns: %s" (Common.list_to_string(settings.OutArchiveFilePatterns));
            sprintf ", OutDirPatterns: %s" (Common.list_to_string(settings.OutDirPatterns));
            sprintf ", OutExtensions: %s" (Common.list_to_string(settings.OutExtensions));
            sprintf ", OutFilePatterns: %s" (Common.list_to_string(settings.OutFilePatterns));
            sprintf ", OutFileTypes: %s" (FileTypesListToString settings.OutFileTypes);
            sprintf ", OutLinesAfterPatterns: %s" (Common.list_to_string(settings.OutLinesAfterPatterns));
            sprintf ", OutLinesBeforePatterns: %s" (Common.list_to_string(settings.OutLinesBeforePatterns));
            sprintf ", PrintResults: %b" settings.PrintResults;
            sprintf ", PrintUsage: %b" settings.PrintUsage;
            sprintf ", PrintVersion: %b" settings.PrintVersion;
            sprintf ", Recursive: %b" settings.Recursive;
            sprintf ", SearchArchives: %b" settings.SearchArchives;
            sprintf ", SearchPatterns: %s" (Common.list_to_string(settings.SearchPatterns));
            sprintf ", StartPath: \"%s\"" settings.StartPath;
            sprintf ", TextFileEncoding: \"%s\"" settings.TextFileEncoding;
            sprintf ", UniqueLines: %b" settings.UniqueLines;
            sprintf ", Verbose: %b" settings.Verbose;
            ")"
        ]
;;
