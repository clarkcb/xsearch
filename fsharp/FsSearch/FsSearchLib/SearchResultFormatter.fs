namespace FsSearchLib

open System
open System.Text.RegularExpressions

open FsFindLib

type SearchResultFormatter (settings : SearchSettings) =
    let fileFormatter = FileResultFormatter(settings)

    let GetRelativeFilePath (result : SearchResult.t) : string =
        result.File.File.ToString()

    let FormatLineMatch (result : SearchResult.t) : string =
        if String.IsNullOrWhiteSpace(result.Line) || settings.MaxLineLength = 0 then
            ""
        else
            let matchLength = result.MatchEndIndex - result.MatchStartIndex

            let matchStartIndex, matchEndIndex, prefix, suffix, colorStartIndex, colorEndIndex =
                if matchLength > settings.MaxLineLength then
                    let prefix = if result.MatchStartIndex > 3 then "..." else ""
                    let suffix = "..."
                    let colorStartIndex = prefix.Length
                    let colorEndIndex = settings.MaxLineLength - 3
                    let matchStartIndex = result.MatchStartIndex - 1 + colorStartIndex
                    let matchEndIndex = result.MatchStartIndex - 1 + colorEndIndex
                    (matchStartIndex, matchEndIndex, prefix, suffix, colorStartIndex, colorEndIndex)
                else
                    let prefix = ""
                    let suffix = ""
                    (result.MatchStartIndex - 1, result.MatchEndIndex - 1, prefix, suffix, 0, matchLength)

        
            let matchString = prefix + result.Line.Substring(matchStartIndex, matchEndIndex - matchStartIndex) + suffix
            if settings.Colorize then
                ColorUtil.Colorize matchString colorStartIndex colorEndIndex settings.LineColor
            else
                matchString
    
    let FormatMatchingLine (result : SearchResult.t) : string =
        if String.IsNullOrWhiteSpace(result.Line) || settings.MaxLineLength = 0 then
            ""
        elif settings.MaxLineLength > 0 && result.MatchEndIndex - result.MatchStartIndex > settings.MaxLineLength then
            FormatLineMatch result
        else
            let matchLength = result.MatchEndIndex - result.MatchStartIndex
            let leadingWhitespaceCount = result.Line.Length - result.Line.TrimStart().Length
            // let mutable lineStartIndex = result.Line.Length - result.Line.TrimStart().Length
            let mutable trailingWhitespaceCount = result.Line.Length - result.Line.TrimEnd().Length
            // let mutable lineEndIndex = result.Line.TrimEnd().Length - 1
            // let matchStartIndex = result.MatchStartIndex - 1 - leadingWhitespaceCount
            // let matchEndIndex = matchStartIndex + matchLength
            
            // let mutable prefix = ""
            // let mutable suffix = ""
            
            let trimmedLength = result.Line.Length - trailingWhitespaceCount - leadingWhitespaceCount

            let rec recGetIndices (lineStartIndex : int) (lineEndIndex : int) (matchStartIndex : int) (matchEndIndex : int) (maxLineLength : int) : int * int * int * int =
                if lineEndIndex - lineStartIndex < maxLineLength then
                    let lsi =
                        if lineStartIndex > 0
                        then lineStartIndex - 1
                        else lineStartIndex
                    let lei =
                        if lineEndIndex - lsi < maxLineLength && lineEndIndex < trimmedLength
                        then lineEndIndex + 1
                        else lineEndIndex
                    let msi, mei =
                        if lineStartIndex > 0
                        then (matchStartIndex + 1, matchEndIndex + 1)
                        else (matchStartIndex, matchEndIndex)
                    recGetIndices lsi lei msi mei maxLineLength
                else
                    (lineStartIndex, lineEndIndex, matchStartIndex, matchEndIndex)

            let maxLineLength =
                if settings.MaxLineLength < 0 then trimmedLength + 1
                else settings.MaxLineLength

            let lineStartIndex, lineEndIndex, matchStartIndex, matchEndIndex =
                if maxLineLength > 0 && trimmedLength > maxLineLength then
                    recGetIndices (result.MatchStartIndex - 1) (result.MatchEndIndex - 1) 0 matchLength maxLineLength
                else
                    (leadingWhitespaceCount, (result.Line.Length - trailingWhitespaceCount), (result.MatchStartIndex - 1), (result.MatchEndIndex - 1))

            let prefix, lineStartIndex =
                if maxLineLength > 0 && trimmedLength > maxLineLength && lineStartIndex > 2 then
                    ("...", lineStartIndex + 3)
                else
                    ("", lineStartIndex)

            let suffix, lineEndIndex =
                if maxLineLength > 0 && trimmedLength > maxLineLength && lineEndIndex < (trimmedLength - 3) then
                    ("...", lineEndIndex - 3)
                else
                    ("", lineEndIndex)

            let formatted = prefix + result.Line.Substring(lineStartIndex, lineEndIndex - lineStartIndex) + suffix
            
            if settings.Colorize then
                ColorUtil.Colorize formatted matchStartIndex matchEndIndex settings.LineColor
            else
                formatted

    let SingleLineFormat (result : SearchResult.t) : string =
        let formattedFileResult = fileFormatter.FormatFileResult(result.File)
        let matchString =
            if result.LineNum = 0 then
                $" matches at [%d{result.MatchStartIndex}:%d{result.MatchEndIndex}]"
            else
                let line = FormatMatchingLine result
                $": %d{result.LineNum}: [%d{result.MatchStartIndex}:%d{result.MatchEndIndex}]: %s{line}"
        formattedFileResult + matchString

    let MultiLineFormat (result : SearchResult.t) : string =
        let hdr = 
            String.concat "\n" [
                $"%s{new string('=', 80)}";
                $"%s{FileResult.ToString(result.File)}: %d{result.LineNum}: [%d{result.MatchStartIndex}:%d{result.MatchEndIndex}]";
                $"%s{new string('-', 80)}";
            ] + "\n"
        let maxLineNum = result.LineNum + (List.length result.LinesAfter)
        let maxLineNumLength = maxLineNum.ToString().Length
        let paddedLineNum (lineNum : int) : string =
            let lineNumString = lineNum.ToString()
            (new string(' ', (maxLineNumLength - lineNumString.Length))) + lineNumString
        let rec recLines (lines : string list) (lineNum : int) (linesString : string) : string =
            match lines with
            | [] -> linesString
            | l :: ls ->
                recLines ls (lineNum + 1) (linesString + $"  %s{paddedLineNum lineNum} | %s{l}\n")
        let linesBeforeString = recLines result.LinesBefore (result.LineNum - (List.length result.LinesBefore)) ""
        let linesAfterString = recLines result.LinesAfter (result.LineNum + 1) ""
        String.concat "" [
            hdr;
            linesBeforeString;
            $"> %s{paddedLineNum result.LineNum} | %s{result.Line}\n";
            linesAfterString;
        ]

    member this.FormatLineWithColor (line : string) : string =
        match (Seq.tryFind (fun p -> (p:Regex).Match(line).Success) settings.SearchPatterns) with
        | Some searchPattern ->
            let lineMatch = searchPattern.Match(line)
            ColorUtil.Colorize line lineMatch.Index (lineMatch.Index + lineMatch.Length) settings.LineColor
        | None -> line

    member this.FormatLineFun =
        if settings.Colorize
        then this.FormatLineWithColor
        else fun (line : string) -> line

    member this.FormatLine (line : string) : string =
        this.FormatLineFun line

    member this.FormatMatchWithColor (m : string) : string =
        ColorUtil.Colorize m 0 m.Length settings.LineColor

    member this.FormatMatchFun =
        if settings.Colorize
        then this.FormatMatchWithColor
        else fun (m : string) -> m

    member this.FormatMatch (m : string) : string =
        this.FormatMatchFun m

    member this.Format (result : SearchResult.t) : string =
        if (not (List.isEmpty result.LinesBefore)) || (not (List.isEmpty result.LinesAfter)) then
            MultiLineFormat result
        else
            SingleLineFormat result
    
    // read-only member properties
    member this.FileFormatter = fileFormatter
;;
