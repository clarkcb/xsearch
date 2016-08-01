package ktsearch

/**
 * @author cary on 7/25/16.
 */
data class SearchResult(val searchPattern: Regex,
                        val file: SearchFile?,
                        val lineNum: Int,
                        val matchStartIndex: Int,
                        val matchEndIndex: Int,
                        val line: String,
                        val linesBefore: List<String>,
                        val linesAfter: List<String>) {

    private val noSearchFileText = "<text>"

    constructor(searchPattern: Regex,
                file: SearchFile?,
                lineNum: Int,
                matchStartIndex: Int,
                matchEndIndex: Int,
                line: String) :
            this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex,
                    line, listOf(), listOf())

    override fun toString(): String {
        if (linesBefore.isNotEmpty() || linesAfter.isNotEmpty()) {
            return multiLineToString()
        } else {
            return singleLineToString()
        }
    }

    fun multiLineToString(): String {
        val lineSepLength = 80
        val fileString = if (file == null) noSearchFileText else file.toString()
        val sb = StringBuilder().
                append("=".repeat(lineSepLength)).append("\n").
                append(fileString).append(": ").append(lineNum).append(": [").
                append(matchStartIndex).append(":").append(matchEndIndex).
                append("]\n").append("-".repeat(lineSepLength)).append("\n")
        var currentLineNum = lineNum
        val lineNumPadding = (lineNum + linesAfter.size).toString().length
        val lineFormat = " %1$" + lineNumPadding + "d | %2\$s\n"
        if (linesBefore.isNotEmpty()) {
            currentLineNum -= linesBefore.size
            for (lineBefore in linesBefore) {
                sb.append(" ").append(String.format(lineFormat, currentLineNum,
                        lineBefore))
                currentLineNum++
            }
        }
        sb.append(">").append(String.format(lineFormat, lineNum, line))
        if (linesAfter.isNotEmpty()) {
            currentLineNum++
            for (lineAfter in linesAfter) {
                sb.append(" ").append(String.format(lineFormat, currentLineNum,
                        lineAfter))
                currentLineNum++
            }
        }
        return sb.toString()
    }

    private fun singleLineToString(): String {
        val sb = StringBuilder()
        if (file != null) {
            sb.append(file.toString())
        } else {
            sb.append(noSearchFileText)
        }

        if (lineNum == 0) {
            sb.append(" matches at [").
                    append(matchStartIndex).
                    append(":").
                    append(matchEndIndex).
                    append("]")
        } else {
            sb.append(": ").
                    append(lineNum).
                    append(": [").
                    append(matchStartIndex).
                    append(":").
                    append(matchEndIndex).
                    append("]: ").
                    append(formatMatchingLine(line))
        }
        return sb.toString()
    }

    private fun formatMatchingLine(line: String): String {
        return line.trim()
    }
}
