package ktsearch

import ktfind.FileResult
import ktfind.FileResultFormatter
import ktfind.SortBy

/**
 * @author cary on 7/25/16.
 */
data class SearchResult(val searchPattern: Regex,
                        val file: FileResult?,
                        val lineNum: Int,
                        val matchStartIndex: Int,
                        val matchEndIndex: Int,
                        val line: String,
                        val linesBefore: List<String>,
                        val linesAfter: List<String>) {

    constructor(searchPattern: Regex,
                file: FileResult?,
                lineNum: Int,
                matchStartIndex: Int,
                matchEndIndex: Int,
                line: String) :
            this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex,
                    line, listOf(), listOf())

    private fun compareSearchResultFields(other: SearchResult): Int {
        var cmp: Int = this.lineNum - other.lineNum
        if (cmp == 0) {
            cmp = this.matchStartIndex - other.matchStartIndex
            if (cmp == 0) {
                cmp = this.matchEndIndex - other.matchEndIndex
            }
        }
        return cmp
    }

    fun compareByPath(other: SearchResult, sortCaseInsensitive: Boolean): Int {
        var fileResultCmp = 0
        if (this.file != null && other.file != null) {
            fileResultCmp = file.compareByPath(other.file, sortCaseInsensitive)
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other)
        }
        return fileResultCmp
    }

    fun compareByName(other: SearchResult, sortCaseInsensitive: Boolean): Int {
        var fileResultCmp = 0
        if (this.file != null && other.file != null) {
            fileResultCmp = file.compareByName(other.file, sortCaseInsensitive)
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other)
        }
        return fileResultCmp
    }

    fun compareBySize(other: SearchResult, sortCaseInsensitive: Boolean): Int {
        var fileResultCmp = 0
        if (this.file != null && other.file != null) {
            fileResultCmp = file.compareBySize(other.file, sortCaseInsensitive)
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other)
        }
        return fileResultCmp
    }

    fun compareByType(other: SearchResult, sortCaseInsensitive: Boolean): Int {
        var fileResultCmp = 0
        if (this.file != null && other.file != null) {
            fileResultCmp = file.compareByType(other.file, sortCaseInsensitive)
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other)
        }
        return fileResultCmp
    }

    fun compareByLastMod(other: SearchResult, sortCaseInsensitive: Boolean): Int {
        var fileResultCmp = 0
        if (this.file != null && other.file != null) {
            fileResultCmp = file.compareByLastMod(other.file, sortCaseInsensitive)
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other)
        }
        return fileResultCmp
    }
}

class SearchResultFormatter(val settings: SearchSettings) {
    private val noSearchFileText = "<text>"
    val fileResultFormatter = FileResultFormatter(settings.findSettings)

    private fun formatLineWithColor(line: String): String {
        var formattedLine = line
        for (p in settings.searchPatterns) {
            val m = p.find(formattedLine)
            if (m != null) {
                formattedLine = colorize(formattedLine, m.range.first, m.range.last + 1)
                break
            }
        }
        return formattedLine
    }

    val formatLine = if (settings.colorize) {
        this::formatLineWithColor
    } else {
        { line: String -> line }
    }

    fun format(result: SearchResult): String {
        return if (settings.linesBefore > 0 || settings.linesAfter > 0) {
            multiLineFormat(result)
        } else {
            singleLineFormat(result)
        }
    }

    private fun colorize(s: String, matchStartIndex: Int, matchEndIndex: Int): String {
        return fileResultFormatter.colorize(s, matchStartIndex, matchEndIndex)
    }

    private fun multiLineFormat(result: SearchResult): String {
        val lineSepLength = 80
        val fileString = if (result.file == null) noSearchFileText else fileResultFormatter.formatFileResult(result.file)
        val sb = StringBuilder().
                append("=".repeat(lineSepLength)).append("\n").
                append(fileString).append(": ").append(result.lineNum).append(": [").
                append(result.matchStartIndex).append(":").append(result.matchEndIndex).
                append("]\n").append("-".repeat(lineSepLength)).append("\n")
        var currentLineNum = result.lineNum
        val lineNumPadding = (result.lineNum + result.linesAfter.size).toString().length
        val lineFormat = " %1$" + lineNumPadding + "d | %2\$s\n"
        if (result.linesBefore.isNotEmpty()) {
            currentLineNum -= result.linesBefore.size
            for (lineBefore in result.linesBefore) {
                sb.append(" ").append(String.format(lineFormat, currentLineNum,
                        lineBefore))
                currentLineNum++
            }
        }
        var line = result.line
        if (settings.colorize) {
            line = colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1)
        }
        sb.append(">").append(String.format(lineFormat, result.lineNum, line))
        if (result.linesAfter.isNotEmpty()) {
            currentLineNum++
            for (lineAfter in result.linesAfter) {
                sb.append(" ").append(String.format(lineFormat, currentLineNum,
                        lineAfter))
                currentLineNum++
            }
        }
        return sb.toString()
    }

    private fun formatMatchingLine(result: SearchResult): String {
        var formatted = result.line.trim()
        val leadingWhitespaceCount = result.line.trimEnd().length - formatted.length
        var formattedLength = formatted.length
        val maxLineEndIndex = formattedLength - 1
        val matchLength = result.matchEndIndex - result.matchStartIndex
        var matchStartIndex = result.matchStartIndex - 1 - leadingWhitespaceCount
        var matchEndIndex = matchStartIndex + matchLength

        if (formattedLength > settings.maxLineLength) {
            var lineStartIndex = matchStartIndex
            var lineEndIndex = lineStartIndex + matchLength
            matchStartIndex = 0
            matchEndIndex = matchLength

            while (lineEndIndex > formattedLength - 1) {
                lineStartIndex--
                lineEndIndex--
                matchStartIndex++
                matchEndIndex++
            }

            formattedLength = lineEndIndex - lineStartIndex
            while (formattedLength < settings.maxLineLength) {
                if (lineStartIndex > 0) {
                    lineStartIndex--
                    matchStartIndex++
                    matchEndIndex++
                    formattedLength = lineEndIndex - lineStartIndex
                }
                if (formattedLength < settings.maxLineLength && lineEndIndex < maxLineEndIndex) {
                    lineEndIndex++
                }
                formattedLength = lineEndIndex - lineStartIndex
            }

            formatted = formatted.substring(lineStartIndex, lineEndIndex)

            if (lineStartIndex > 2) {
                formatted = "..." + formatted.substring(3)
            }
            if (lineEndIndex < maxLineEndIndex - 3) {
                formatted = formatted.substring(0, formattedLength - 3) + "..."
            }
        }

        if (settings.colorize) {
            formatted = colorize(formatted, matchStartIndex, matchEndIndex)
        }

        return formatted
    }

    private fun singleLineFormat(result: SearchResult): String {
        val sb = StringBuilder()
        if (result.file != null) {
            sb.append(fileResultFormatter.formatFileResult(result.file))
        } else {
            sb.append(noSearchFileText)
        }

        if (result.lineNum == 0) {
            sb.append(" matches at [").
                    append(result.matchStartIndex).
                    append(":").
                    append(result.matchEndIndex).
                    append("]")
        } else {
            sb.append(": ").
                    append(result.lineNum).
                    append(": [").
                    append(result.matchStartIndex).
                    append(":").
                    append(result.matchEndIndex).
                    append("]: ").
                    append(formatMatchingLine(result))
        }
        return sb.toString()
    }
}

class SearchResultSorter(val settings: SearchSettings) {
    private fun getSearchResultComparator(): Comparator<SearchResult> {
        return if (settings.sortDescending) {
            when (settings.sortBy) {
                SortBy.FILENAME -> Comparator { sr1, sr2 ->
                    sr2.compareByName(
                        sr1,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILESIZE -> Comparator { sr1, sr2 ->
                    sr2.compareBySize(
                        sr1,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILETYPE -> Comparator { sr1, sr2 ->
                    sr2.compareByType(
                        sr1,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.LASTMOD -> Comparator { sr1, sr2 ->
                    sr2.compareByLastMod(
                        sr1,
                        settings.sortCaseInsensitive
                    )
                }
                else -> Comparator { sr1, sr2 -> sr2.compareByPath(sr1, settings.sortCaseInsensitive) }
            }
        } else {
            when (settings.sortBy) {
                SortBy.FILENAME -> Comparator { sr1, sr2 ->
                    sr1.compareByName(
                        sr2,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILESIZE -> Comparator { sr1, sr2 ->
                    sr1.compareBySize(
                        sr2,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILETYPE -> Comparator { sr1, sr2 ->
                    sr1.compareByType(
                        sr2,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.LASTMOD -> Comparator { sr1, sr2 ->
                    sr1.compareByLastMod(
                        sr2,
                        settings.sortCaseInsensitive
                    )
                }
                else -> Comparator { sr1, sr2 -> sr1.compareByPath(sr2, settings.sortCaseInsensitive) }
            }
        }
    }

    fun sort(searchResults: List<SearchResult>): List<SearchResult> {
        if (searchResults.isEmpty()) {
            return emptyList()
        }
        val searchResultComparator = getSearchResultComparator()
        return searchResults.stream().sorted(searchResultComparator).toList()
    }
}
