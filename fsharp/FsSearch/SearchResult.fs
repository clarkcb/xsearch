namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

type SearchResult(searchPattern : Regex, file : FileInfo, lineNum : int, line : string) =
    let _searchPattern = searchPattern
    let _file = file
    let _lineNum = lineNum
    let _line = line

    // read-only member properties
    member this.SearchPattern = _searchPattern
    member this.File = _file
    member this.LineNum = _lineNum
    member this.Line = _line


    override this.ToString() =
        let matchString =
            if _lineNum = 0 then
                sprintf " has match for pattern \"%s\"" (_searchPattern.ToString())
            else
                sprintf ": %d: %s" _lineNum (_line.Trim())
        let sb =
            (new StringBuilder()).
                Append(_file.FullName).
                Append(matchString)
        sb.ToString()
    ;;
