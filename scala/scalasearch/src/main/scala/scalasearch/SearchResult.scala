package scalasearch

import scala.util.matching.Regex

case class StringSearchResult(searchPattern: Regex, lineNum:Int,
                              matchStartIndex:Int, matchEndIndex:Int, line:String,
                              linesBefore: List[String], linesAfter: List[String])

class SearchResult(val searchPattern: Regex, val file: SearchFile,
                   val lineNum: Int, val matchStartIndex:Int,
                   val matchEndIndex:Int, val line: String,
                   val linesBefore: List[String], val linesAfter: List[String],
                   maxLineLength:Int=DefaultSettings.maxLineLength) {

  def this(searchPattern:Regex, file:SearchFile, lineNum:Int, matchStartIndex:Int,
           matchEndIndex:Int, line:String) = {
    this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
      List.empty[String], List.empty[String])
  }

  val sepLen = 80

  override def toString = {
    if (linesBefore.nonEmpty || linesAfter.nonEmpty)
      multilineToString
    else
      singleLineToString
  }

  def singleLineToString = {
    val matchString = file.fileType match {
      case FileType.Text =>
        ": %d: [%d:%d]: %s".format(lineNum, matchStartIndex, matchEndIndex,
          formatMatchingLine)
      case _ => " matches"
    }
    file.getPathWithContainers + matchString
  }

  private def formatMatchingLine:String = {
    val lineLength = line.length
    val matchLength = matchEndIndex - matchStartIndex
    val formatted =
      if (lineLength > maxLineLength) {
        var adjustedMaxLength = maxLineLength - matchLength
        var beforeIndex = matchStartIndex
        if (matchStartIndex > 0) {
          beforeIndex -= (adjustedMaxLength / 4)
          if (beforeIndex < 0) beforeIndex = 0
        }
        adjustedMaxLength -= (matchStartIndex - beforeIndex)
        var afterIndex = matchEndIndex + adjustedMaxLength
        if (afterIndex > lineLength) afterIndex = lineLength
        val before =
          if (beforeIndex > 3) {
            beforeIndex += 3
            "..."
          } else ""
        val after =
          if (afterIndex < lineLength - 3) {
            afterIndex -= 3
            "..."
          } else ""
        before + line.substring(beforeIndex, afterIndex) + after
      } else {
        line
      }
    formatted.trim
  }

  def lineNumPadding: Int = {
    val maxLineNum = lineNum + linesAfter.length
    "%d".format(maxLineNum).length
  }

  val newLineChars = Set('\n', '\r')

  def trimNewLines(s:String):String = {
    if (s == "") ""
    if (newLineChars.contains(s.last)) trimNewLines(s.init)
    else s
  }

  def multilineToString = {
    val sb = new StringBuilder
    sb.append("%s\n%s: %d: [%d:%d]\n%s\n".format("=" * sepLen,
      file.getPathWithContainers, lineNum, matchStartIndex, matchEndIndex,
      "-" * sepLen))
    val lineFormat = " %1$" + lineNumPadding + "d | %2$s\n"
    var currentLineNum = lineNum
    if (linesBefore.length > 0) {
      currentLineNum -= linesBefore.length
      for (lineBefore <- linesBefore) {
        sb.append(" " + lineFormat.format(currentLineNum, trimNewLines(lineBefore)))
        currentLineNum += 1
      }
    }
    sb.append(">" + lineFormat.format(lineNum, trimNewLines(line)))
    if (linesAfter.length > 0) {
      currentLineNum += 1
      for (lineAfter <- linesAfter) {
        sb.append(" " + lineFormat.format(currentLineNum, trimNewLines(lineAfter)))
        currentLineNum += 1
      }
    } else sb.append('\n')
    sb.toString()
  }
}
