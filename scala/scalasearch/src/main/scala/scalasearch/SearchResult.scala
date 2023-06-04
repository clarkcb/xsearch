package scalasearch

import scalafind.FileResult

import scala.util.matching.Regex

case class SearchResult(searchPattern: Regex, file: Option[FileResult],
                        lineNum: Int, matchStartIndex: Int,
                        matchEndIndex: Int, line: Option[String],
                        linesBefore: Seq[String], linesAfter: Seq[String]) {

  def this(searchPattern: Regex, file: Option[FileResult], lineNum: Int,
           matchStartIndex: Int, matchEndIndex: Int, line: Option[String]) = {
    this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
      Seq.empty[String], Seq.empty[String])
  }
}

class SearchResultFormatter(val settings: SearchSettings) {
  val sepLen = 80

  def format(result: SearchResult): String = {
    if (result.linesBefore.nonEmpty || result.linesAfter.nonEmpty) {
      multiLineFormat(result)
    } else {
      singleLineFormat(result)
    }
  }

  private def colorize(s: String, matchStartIndex: Int, matchEndIndex: Int): String = {
    s.substring(0, matchStartIndex) +
      Color.GREEN +
      s.substring(matchStartIndex, matchEndIndex) +
      Color.RESET +
      s.substring(matchEndIndex)
  }

  private def formatMatchingLine(result: SearchResult): String = {
    result.line match {
      case Some(line) =>
        val leadingWhitespaceCount = line.takeWhile(c => Character.isWhitespace(c)).length
        var formatted = line.trim
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
            lineStartIndex -= 1
            lineEndIndex -= 1
            matchStartIndex += 1
            matchEndIndex += 1
          }

          formattedLength = lineEndIndex - lineStartIndex
          while (formattedLength < settings.maxLineLength) {
            if (lineStartIndex > 0) {
              lineStartIndex -= 1
              matchStartIndex += 1
              matchEndIndex += 1
              formattedLength = lineEndIndex - lineStartIndex
            }
            if (formattedLength < settings.maxLineLength && lineEndIndex < maxLineEndIndex) {
              lineEndIndex += 1
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

        formatted

      case None => ""
    }
  }

  def singleLineFormat(result: SearchResult): String = {
    val filepath = if (result.file.isDefined) result.file.get.toString else "<text>"
    val matchString =
      if (result.lineNum > 0) {
        ": %d: [%d:%d]: %s".format(result.lineNum, result.matchStartIndex, result.matchEndIndex,
          formatMatchingLine(result))
      } else {
        " matches at [%d:%d]".format(result.matchStartIndex, result.matchEndIndex)
      }
    filepath + matchString
  }

  def lineNumPadding(result: SearchResult): Int = {
    val maxLineNum = result.lineNum + result.linesAfter.length
    "%d".format(maxLineNum).length
  }

  val newLineChars = Set('\n', '\r')

  def trimNewLines(s:String): String = {
    if (s == "") {
      ""
    } else if (newLineChars.contains(s.last)) {
      trimNewLines(s.init)
    } else {
      s
    }
  }

  def multiLineFormat(result: SearchResult): String = {
    result.line match {
      case Some(resultLine) =>
        val sb = new StringBuilder
        val filepath = if (result.file.isDefined) result.file.get.toString else "<text>"
        sb.append("%s\n%s: %d: [%d:%d]\n%s\n".format("=" * sepLen, filepath,
          result.lineNum, result.matchStartIndex, result.matchEndIndex, "-" * sepLen))
        val lineFormat = " %1$" + lineNumPadding(result) + "d | %2$s\n"
        var currentLineNum = result.lineNum
        if (result.linesBefore.nonEmpty) {
          currentLineNum -= result.linesBefore.length
          result.linesBefore.foreach { lineBefore =>
            sb.append(" " + lineFormat.format(currentLineNum, trimNewLines(lineBefore)))
            currentLineNum += 1
          }
        }
        var line = trimNewLines(resultLine)
        if (settings.colorize) {
          line = colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1)
        }
        sb.append(">" + lineFormat.format(result.lineNum, line))
        if (result.linesAfter.nonEmpty) {
          currentLineNum += 1
          result.linesAfter.foreach { lineAfter =>
            sb.append(" " + lineFormat.format(currentLineNum, trimNewLines(lineAfter)))
            currentLineNum += 1
          }
        }
        sb.toString()

      case None => ""
    }
  }
}
