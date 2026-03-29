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
    member val LineColor : Color = Color.Green with get, set
    member val LinesAfter = 0 with get, set
    member val LinesAfterToPatterns : Regex list = [] with get, set
    member val LinesAfterUntilPatterns : Regex list = [] with get, set
    member val LinesBefore = 0 with get, set
    member val MaxLineLength = 150 with get, set
    member val MultiLineSearch : bool = false with get, set
    member val OutLinesAfterPatterns : Regex list = [] with get, set
    member val OutLinesBeforePatterns : Regex list = [] with get, set
    member val PrintLines : bool = false with get, set
    member val PrintMatches : bool = false with get, set
    member val PrintResults : bool = false with get, set
    member val SearchArchives : bool = false with get, set
    member val SearchPatterns : Regex list = [] with get, set
    member val TextFileEncoding = "utf-8" with get, set
    member val UniqueLines : bool = false with get, set
;;
