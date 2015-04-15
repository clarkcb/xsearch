namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

type SearchResult(searchPattern : Regex, file : FileInfo, lineNum : int,
                  matchStartIndex : int, matchEndIndex : int, line : string,
                  linesBefore : string list, linesAfter : string list) =
    member val SearchPattern = searchPattern with get,set
    member val File = file with get,set
    member val LineNum = lineNum with get,set
    member val MatchStartIndex = matchStartIndex with get,set
    member val MatchEndIndex = matchEndIndex with get,set
    member val Line = line with get,set
    member val LinesBefore = [] with get,set
    member val LinesAfter = [] with get,set

    override this.ToString() =
        if this.LinesBefore.Length > 0 || this.LinesAfter.Length > 0 then
            this.SingleLineToString()
        else
            this.SingleLineToString()

    // TODO
    member this.MultLineToString() =
        this.SingleLineToString

    member this.SingleLineToString() =
        let matchString =
            if this.LineNum = 0 then
                sprintf " has match for pattern \"%s\"" (this.SearchPattern.ToString())
            else
                sprintf ": %d: [%d:%d]: %s" this.LineNum this.MatchStartIndex this.MatchEndIndex (this.Line.Trim())
        let sb =
            (new StringBuilder()).
                Append(this.File.FullName).
                Append(matchString)
        sb.ToString()


    ;;
