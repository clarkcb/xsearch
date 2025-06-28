namespace FsSearchLib

open System.Text.RegularExpressions
open FsFindLib

module SearchResult =
    type t = {
        SearchPattern : Regex;
        File : FileResult.t;
        LineNum : int;
        MatchStartIndex : int;
        MatchEndIndex : int;
        Line : string;
        LinesBefore : string list;
        LinesAfter : string list;
    }

    let Create (pattern : Regex) (lineNum : int) (startIndex : int) (endIndex : int) (line : string) (linesBefore : string list) (linesAfter : string list) : t =
        {
            SearchPattern = pattern;
            File = { Containers=[]; File=null; FileType=FileType.Unknown; };
            LineNum = lineNum;
            MatchStartIndex = startIndex;
            MatchEndIndex = endIndex;
            Line = line;
            LinesBefore = linesBefore;
            LinesAfter = linesAfter;
        }
;;
