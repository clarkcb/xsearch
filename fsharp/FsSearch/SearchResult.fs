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
    member val LinesBefore = linesBefore with get,set
    member val LinesAfter = linesAfter with get,set

    override this.ToString() : string =
        if this.LinesBefore.Length > 0 || this.LinesAfter.Length > 0 then
            this.MultLineToString()
        else
            this.SingleLineToString()

    member this.LineNumPadding() : int =
        let maxLineNum = this.LineNum + this.LinesAfter.Length
        (sprintf "%d" maxLineNum).Length

    member this.MultLineToString() : string =
        let lines = new List<String>()
        lines.Add(sprintf "%s\n%s: %d: [%d:%d]\n%s"
                      (new String('=', 80))
                      this.File.FullName
                      this.LineNum
                      this.MatchStartIndex
                      this.MatchEndIndex
                      (new String('-', 80)))
        let mutable currentLineNum = this.LineNum
        let lineFormat = sprintf " {0,%d} | {1}" (this.LineNumPadding())
        if this.LinesBefore.Length > 0 then
            currentLineNum <- this.LineNum - this.LinesBefore.Length
            for lineBefore in this.LinesBefore do
                lines.Add(String.Format(" "+lineFormat, currentLineNum, lineBefore))
                currentLineNum <- currentLineNum + 1
        lines.Add(String.Format(">" + lineFormat, this.LineNum, this.Line))
        if this.LinesAfter.Length > 0 then
            currentLineNum <- this.LineNum + 1
            for lineAfter in this.LinesAfter do
                lines.Add(String.Format(" "+lineFormat, currentLineNum, lineAfter))
                currentLineNum <- currentLineNum + 1
        lines.Add("")
        lines |> List.ofSeq |> String.concat "\n" 

    member this.SingleLineToString() : string =
        let matchString =
            if this.LineNum = 0 then
                sprintf " matches at [%d:%d]" this.MatchStartIndex this.MatchEndIndex
            else
                sprintf ": %d: [%d:%d]: %s" this.LineNum this.MatchStartIndex this.MatchEndIndex (this.Line.Trim())
        let sb =
            (new StringBuilder()).
                Append(this.File.FullName).
                Append(matchString)
        sb.ToString()

    member this.SortKey = 
        (this.File.DirectoryName.ToLower(), this.File.Name.ToLower(), this.LineNum, this.MatchStartIndex)

    ;;
