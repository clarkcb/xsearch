namespace FsSearchLib

open FsFind

type SearchResultFormatter (settings : SearchSettings) =

    let Colorize (s : string) (matchStartIndex : int) (matchEndIndex : int) : string =
        let matchLength = matchEndIndex - matchStartIndex
        s.Substring(0, matchStartIndex) +
            Color.Green + 
            s.Substring(matchStartIndex, matchLength) +
            Color.Reset + 
            s.Substring(matchStartIndex + matchLength)

    let GetRelativeFilePath (result : SearchResult.t) : string =
        result.File.File.ToString()

    let FormatMatchingLine (result : SearchResult.t) : string =
        let mutable formatted : string = result.Line.Trim()
        let mutable formattedLength = formatted.Length
        let leadingWhitespaceCount = result.Line.TrimEnd().Length - formattedLength
        let maxLineEndIndex = formattedLength - 1
        let matchLength = result.MatchEndIndex - result.MatchStartIndex
        let matchStartIndex = result.MatchStartIndex - 1 - leadingWhitespaceCount
        let matchEndIndex = matchStartIndex + matchLength
        let rec recGetIndices (lineStartIndex : int) (lineEndIndex : int) (matchStartIndex : int) (matchEndIndex : int) : int * int * int * int =
            if lineEndIndex - lineStartIndex < settings.MaxLineLength then
                let lsi =
                    if lineStartIndex > 0
                    then lineStartIndex - 1
                    else lineStartIndex
                let lei =
                    if lineEndIndex - lsi < settings.MaxLineLength && lineEndIndex < maxLineEndIndex
                    then lineEndIndex + 1
                    else lineEndIndex
                let msi, mei =
                    if lineStartIndex > 0
                    then (matchStartIndex + 1, matchEndIndex + 1)
                    else (matchStartIndex, matchEndIndex)
                recGetIndices lsi lei msi mei
            else
                (lineStartIndex, lineEndIndex, matchStartIndex, matchEndIndex)
        let lineStartIndex, lineEndIndex, matchStartIndex, matchEndIndex =
            if formattedLength > settings.MaxLineLength then
                recGetIndices matchStartIndex matchEndIndex matchStartIndex matchEndIndex
            else
                (0, maxLineEndIndex, matchStartIndex, matchEndIndex)
        formattedLength <- if lineStartIndex = 0 then lineEndIndex - lineStartIndex + 1 else lineEndIndex - lineStartIndex
        formatted <- formatted.Substring(lineStartIndex, formattedLength)
        if lineStartIndex > 2 then
            formatted <- "..." + formatted.Substring(3)
        if lineEndIndex < maxLineEndIndex - 3 then
            formatted <- formatted.Substring(0, formattedLength - 3) + "..."
        if settings.Colorize then
            formatted <- Colorize formatted matchStartIndex matchEndIndex
        formatted

    let SingleLineFormat (result : SearchResult.t) : string =
        let matchString =
            if result.LineNum = 0 then
                $" matches at [%d{result.MatchStartIndex}:%d{result.MatchEndIndex}]"
            else
                let line = FormatMatchingLine result
                $": %d{result.LineNum}: [%d{result.MatchStartIndex}:%d{result.MatchEndIndex}]: %s{line}"
        (GetRelativeFilePath result) + matchString

    let MultiLineFormat (result : SearchResult.t) : string =
        let hdr = 
            String.concat "\n" [
                $"%s{new string('=', 80)}";
                $"%s{GetRelativeFilePath result}: %d{result.LineNum}: [%d{result.MatchStartIndex}:%d{result.MatchEndIndex}]";
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

    member this.Format (result : SearchResult.t) : string =
        if (not (List.isEmpty result.LinesBefore)) || (not (List.isEmpty result.LinesAfter)) then
            MultiLineFormat result
        else
            SingleLineFormat result
;;
