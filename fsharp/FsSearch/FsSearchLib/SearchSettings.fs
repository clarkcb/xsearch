namespace FsSearchLib

open System.Text.RegularExpressions
open FsFindLib

type SearchSettings() =
    inherit FindSettings()
    member this.ArchivesOnly
        with get () = base.ArchivesOnly
        and set value =
            base.ArchivesOnly <- value
            if value then
                this.SearchArchives <- value

    member val FirstMatch : bool = false with get, set
    member val InLinesAfterPatterns : Regex list = [] with get, set
    member val InLinesBeforePatterns : Regex list = [] with get, set
    member val LinesAfter = 0 with get, set
    member val LinesAfterToPatterns : Regex list = [] with get, set
    member val LinesAfterUntilPatterns : Regex list = [] with get, set
    member val LinesBefore = 0 with get, set
    member val MaxLineLength = 150 with get, set
    member val MultiLineSearch : bool = false with get, set
    member val OutLinesAfterPatterns : Regex list = [] with get, set
    member val OutLinesBeforePatterns : Regex list = [] with get, set
    member val PrintLines : bool = false with get, set
    member val PrintResults : bool = false with get, set
    member val SearchArchives : bool = false with get, set
    member val SearchPatterns : Regex list = [] with get, set
    member val TextFileEncoding = "utf-8" with get, set
    member val UniqueLines : bool = false with get, set

    member this.ToString =
        String.concat "" [
            "SearchSettings(";
            $"ArchivesOnly: %b{this.ArchivesOnly}";
            $", Colorize: %b{this.Colorize}";
            $", Debug: %b{this.Debug}";
            $", FirstMatch: %b{this.FirstMatch}";
            $", FollowSymlinks: %b{this.FollowSymlinks}";
            $", InArchiveExtensions: %s{Common.ListToString(this.InArchiveExtensions)}";
            $", InArchiveFilePatterns: %s{Common.ListToString(this.InArchiveFilePatterns)}";
            $", IncludeHidden: %b{this.IncludeHidden}";
            $", InDirPatterns: %s{Common.ListToString(this.InDirPatterns)}";
            $", InExtensions: %s{Common.ListToString(this.InExtensions)}";
            $", InFilePatterns: %s{Common.ListToString(this.InFilePatterns)}";
            $", InFileTypes: %s{this.FileTypesListToString this.InFileTypes}";
            $", InLinesAfterPatterns: %s{Common.ListToString(this.InLinesAfterPatterns)}";
            $", InLinesBeforePatterns: %s{Common.ListToString(this.InLinesBeforePatterns)}";
            $", LinesAfter: %d{this.LinesAfter}";
            $", LinesAfterToPatterns: %s{Common.ListToString(this.LinesAfterToPatterns)}";
            $", LinesAfterUntilPatterns: %s{Common.ListToString(this.LinesAfterUntilPatterns)}";
            $", LinesBefore: %d{this.LinesBefore}";
            $", MaxDepth: %i{this.MaxDepth}";
            $", MaxLastMod: %s{this.DateTimeOptionListToString this.MaxLastMod}";
            $", MaxLineLength: %d{this.MaxLineLength}";
            $", MaxSize: %i{this.MaxSize}";
            $", MinDepth: %i{this.MinDepth}";
            $", MinLastMod: %s{this.DateTimeOptionListToString this.MinLastMod}";
            $", MinSize: %i{this.MinSize}";
            $", MultiLineSearch: %b{this.MultiLineSearch}";
            $", OutArchiveExtensions: %s{Common.ListToString(this.OutArchiveExtensions)}";
            $", OutArchiveFilePatterns: %s{Common.ListToString(this.OutArchiveFilePatterns)}";
            $", OutDirPatterns: %s{Common.ListToString(this.OutDirPatterns)}";
            $", OutExtensions: %s{Common.ListToString(this.OutExtensions)}";
            $", OutFilePatterns: %s{Common.ListToString(this.OutFilePatterns)}";
            $", OutFileTypes: %s{this.FileTypesListToString this.OutFileTypes}";
            $", OutLinesAfterPatterns: %s{Common.ListToString(this.OutLinesAfterPatterns)}";
            $", OutLinesBeforePatterns: %s{Common.ListToString(this.OutLinesBeforePatterns)}";
            $", Paths: %s{Common.ListToString(this.Paths)}";
            $", PrintDirs: %b{this.PrintDirs}";
            $", PrintFiles: %b{this.PrintFiles}";
            $", PrintLines: %b{this.PrintLines}";
            $", PrintResults: %b{this.PrintResults}";
            $", PrintUsage: %b{this.PrintUsage}";
            $", PrintVersion: %b{this.PrintVersion}";
            $", Recursive: %b{this.Recursive}";
            $", SearchArchives: %b{this.SearchArchives}";
            $", SearchPatterns: %s{Common.ListToString(this.SearchPatterns)}";
            $", SortBy: %s{SortUtil.NameFromSortBy(this.SortBy)}";
            $", SortCaseInsensitive: %b{this.SortCaseInsensitive}";
            $", SortDescending: %b{this.SortDescending}";
            $", TextFileEncoding: \"%s{this.TextFileEncoding}\"";
            $", UniqueLines: %b{this.UniqueLines}";
            $", Verbose: %b{this.Verbose}";
            ")"
        ]
;;
