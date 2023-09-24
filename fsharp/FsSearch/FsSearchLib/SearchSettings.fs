namespace FsSearchLib

open System.Text.RegularExpressions
open FsFind

type SearchSettings() =
    inherit FindSettings()
    member this.ArchivesOnly
        with get () = base.ArchivesOnly
        and set value =
            base.ArchivesOnly <- value
            if value then
                this.SearchArchives <- value

    member val Colorize : bool = true with get, set
    member val FirstMatch : bool = false with get, set
    member val InLinesAfterPatterns : Regex list = [] with get, set
    member val InLinesBeforePatterns : Regex list = [] with get, set
    member val LinesAfter = 0 with get, set
    member val LinesAfterToPatterns : Regex list = [] with get, set
    member val LinesAfterUntilPatterns : Regex list = [] with get, set
    member val LinesBefore = 0 with get, set
    member val ListLines : bool = false with get, set
    member val MaxLineLength = 150 with get, set
    member val MultiLineSearch : bool = false with get, set
    member val OutLinesAfterPatterns : Regex list = [] with get, set
    member val OutLinesBeforePatterns : Regex list = [] with get, set
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
            $", ExcludeHidden: %b{this.ExcludeHidden}";
            $", FirstMatch: %b{this.FirstMatch}";
            $", InArchiveExtensions: %s{Common.ListToString(this.InArchiveExtensions)}";
            $", InArchiveFilePatterns: %s{Common.ListToString(this.InArchiveFilePatterns)}";
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
            $", ListDirs: %b{this.ListDirs}";
            $", ListFiles: %b{this.ListFiles}";
            $", ListLines: %b{this.ListLines}";
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
