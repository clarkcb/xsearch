namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

module SearchResult =
    type t = {
        SearchPattern : Regex;
        File : SearchFile.t;
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

    let SingleLineToString (sr : t) : string =
        let matchString =
            if sr.LineNum = 0 then
                sprintf " matches at [%d:%d]" sr.MatchStartIndex sr.MatchEndIndex
            else
                sprintf ": %d: [%d:%d]: %s" sr.LineNum sr.MatchStartIndex sr.MatchEndIndex (sr.Line.Trim())
        (SearchFile.ToString sr.File) + matchString

    let MultLineToString (sr : t) : string =
        let hdr = 
            String.concat "\n" [
                sprintf "%s" (new String('=', 80));
                sprintf "%s: %d: [%d:%d]" (SearchFile.ToString sr.File) sr.LineNum sr.MatchStartIndex sr.MatchEndIndex;
                sprintf "%s" (new String('-', 80));
            ] + "\n"
        let maxLineNum = sr.LineNum + (List.length sr.LinesAfter)
        let maxLineNumLength = maxLineNum.ToString().Length
        let paddedLineNum (lineNum : int) : string =
            let lineNumString = lineNum.ToString()
            (new String(' ', (maxLineNumLength - lineNumString.Length))) + lineNumString
        let rec recLines (lines : string list) (lineNum : int) (linesString : string) : string =
            match lines with
            | [] -> linesString
            | l :: ls ->
                recLines ls (lineNum + 1) (linesString + (sprintf "  %s | %s\n" (paddedLineNum lineNum) l))
        let linesBeforeString = recLines sr.LinesBefore (sr.LineNum - (List.length sr.LinesBefore)) ""
        let linesAfterString = recLines sr.LinesAfter (sr.LineNum + 1) ""
        String.concat "" [
            hdr;
            linesBeforeString;
            sprintf "> %s | %s\n" (paddedLineNum sr.LineNum) sr.Line;
            linesAfterString;
        ]

    let ToString (sr : t) : string =
        if (not (List.isEmpty sr.LinesBefore)) || (not (List.isEmpty sr.LinesAfter)) then
            MultLineToString sr
        else
            SingleLineToString sr
;;