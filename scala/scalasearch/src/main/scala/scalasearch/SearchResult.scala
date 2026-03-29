package scalasearch

import scalafind.Color.Color
import scalafind.{FileResult, FileResultFormatter, SortBy}

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
  
  def compareBySearchFields(other: SearchResult): Int = {
    if (this.lineNum == other.lineNum) {
      if (this.matchStartIndex == other.matchStartIndex) {
        if (this.matchEndIndex < other.matchEndIndex) -1 else 1
      } else {
        if (this.matchStartIndex < other.matchStartIndex) -1 else 1
      }
    } else {
      if (this.lineNum < other.lineNum) -1 else 1
    }
  }

  def compareByPath(other: SearchResult, sortCaseInsensitive: Boolean): Int = {
    val fileCmp: Int = (this.file, other.file) match {
      case (Some(thisFile), Some(otherFile)) =>
        thisFile.compareByPath(otherFile, sortCaseInsensitive)
      case (_, _) => 0
    }
    if (fileCmp == 0) {
      compareBySearchFields(other)
    } else {
      fileCmp
    }
  }

  def beforeByPath(other: SearchResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByPath(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareByName(other: SearchResult, sortCaseInsensitive: Boolean): Int = {
    val fileCmp: Int = (this.file, other.file) match {
      case (Some(thisFile), Some(otherFile)) =>
        thisFile.compareByName(otherFile, sortCaseInsensitive)
      case (_, _) => 0
    }
    if (fileCmp == 0) {
      compareBySearchFields(other)
    } else {
      fileCmp
    }
  }

  def beforeByName(other: SearchResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByName(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareBySize(other: SearchResult, sortCaseInsensitive: Boolean): Int = {
    val fileCmp: Int = (this.file, other.file) match {
      case (Some(thisFile), Some(otherFile)) =>
        thisFile.compareBySize(otherFile, sortCaseInsensitive)
      case (_, _) => 0
    }
    if (fileCmp == 0) {
      compareBySearchFields(other)
    } else {
      fileCmp
    }
  }

  def beforeBySize(other: SearchResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareBySize(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareByType(other: SearchResult, sortCaseInsensitive: Boolean): Int = {
    val fileCmp: Int = (this.file, other.file) match {
      case (Some(thisFile), Some(otherFile)) =>
        thisFile.compareByType(otherFile, sortCaseInsensitive)
      case (_, _) => 0
    }
    if (fileCmp == 0) {
      compareBySearchFields(other)
    } else {
      fileCmp
    }
  }

  def beforeByType(other: SearchResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByType(other, sortCaseInsensitive)
    cmp < 0
  }

  def compareByLastMod(other: SearchResult, sortCaseInsensitive: Boolean): Int = {
    val fileCmp: Int = (this.file, other.file) match {
      case (Some(thisFile), Some(otherFile)) =>
        thisFile.compareByLastMod(otherFile, sortCaseInsensitive)
      case (_, _) => 0
    }
    if (fileCmp == 0) {
      compareBySearchFields(other)
    } else {
      fileCmp
    }
  }

  def beforeByLastMod(other: SearchResult, sortCaseInsensitive: Boolean): Boolean = {
    val cmp = compareByLastMod(other, sortCaseInsensitive)
    cmp < 0
  }
}

class SearchResultFormatter(val settings: SearchSettings) {
  val sepLen = 80
  var fileResultFormatter = new FileResultFormatter(settings.getFindSettings)

  private def formatLineWithColor(line: String): String = {
    var formattedLine = line
    settings.searchPatterns.flatMap(p => p.findFirstMatchIn(formattedLine)).take(1).foreach { m =>
      formattedLine = colorize(formattedLine, m.start, m.end, settings.lineColor)
    }
    formattedLine
  }

  val formatLine: String => String =
    if (settings.colorize) {
      formatLineWithColor
    } else {
      (line: String) => line
    }

  private def formatMatchWithColor(m: String): String = {
    colorize(m, 0, m.length, settings.lineColor)
  }

  val formatMatch: String => String =
    if (settings.colorize) {
      formatMatchWithColor
    } else {
      (m: String) => m
    }

  def format(result: SearchResult): String = {
    if (result.linesBefore.nonEmpty || result.linesAfter.nonEmpty) {
      multiLineFormat(result)
    } else {
      singleLineFormat(result)
    }
  }

  private def colorize(s: String, matchStartIndex: Int, matchEndIndex: Int, color: Color): String = {
    fileResultFormatter.colorize(s, matchStartIndex, matchEndIndex, color)
  }

  private def formatResultMatch(result: SearchResult): String = {
    result.line match {
      case Some(line) =>
        var matchStartIndex = result.matchStartIndex - 1
        var matchEndIndex = result.matchEndIndex - 1
        val matchLength = matchEndIndex - matchStartIndex

        var prefix = ""
        var suffix = ""
        var colorStartIndex = 0
        var colorEndIndex = matchLength

        if (matchLength > settings.maxLineLength) {
          prefix = if (matchStartIndex > 2) "..." else ""
          suffix = "..."
          colorStartIndex = prefix.length
          colorEndIndex = settings.maxLineLength - 3
          matchEndIndex = matchStartIndex + colorEndIndex
          matchStartIndex = matchStartIndex + colorStartIndex
        }

        val matchString = prefix + line.substring(matchStartIndex, matchEndIndex) + suffix

        if (settings.colorize) {
          colorize(matchString, colorStartIndex, colorEndIndex, settings.lineColor)
        } else {
          matchString
        }
      case None => ""
    }
  }

  private def leadingWhitespaceCount(s: String): Int = {
    var idx = 0
    while (idx < s.length && Character.isWhitespace(s.charAt(idx))) {
      idx += 1
    }
    idx
  }

  private def trailingWhitespaceCount(s: String): Int = {
    var idx = s.length - 1
    while (idx > 0 && Character.isWhitespace(s.charAt(idx))) {
      idx -= 1
    }
    s.length - 1 - idx
  }

  private def getLineIndices(lineStartIdx: Int, lineEndIdx: Int, matchStartIdx: Int, matchEndIdx: Int, maxIdx: Int, maxLength: Int): (Int, Int, Int, Int) = {
    if (maxLength == 0 || lineEndIdx - lineStartIdx == 0) {
      (0, 0, 0, 0)
    } else if (lineEndIdx - lineStartIdx < maxLength) {
      val (lsi, msi, mei) =
        if (lineStartIdx > 0) {
          (lineStartIdx - 1, matchStartIdx + 1, matchEndIdx + 1)
        } else {
          (lineStartIdx, matchStartIdx, matchEndIdx)
        }
      val lei = if (lineEndIdx - lsi < maxLength && lineEndIdx < maxIdx) lineEndIdx + 1 else lineEndIdx
      getLineIndices(lsi, lei, msi, mei, maxIdx, maxLength)
    } else {
      (lineStartIdx, lineEndIdx, matchStartIdx, matchEndIdx)
    }
  }

  private def formatResultLineWithMatch(result: SearchResult): String = {
    if (result.line.isEmpty || result.line.get.trim.isEmpty || settings.maxLineLength == 0) {
      ""
    } else {
      val line = result.line.get
      val matchLength = result.matchEndIndex - result.matchStartIndex
      val maxLineLength =
        if (settings.maxLineLength < 0) {
          result.line.get.length + 1
        } else {
          settings.maxLineLength
        }

      val lineStartIdx = leadingWhitespaceCount(line)
      val lineEndIdx = line.length - 1 - trailingWhitespaceCount(line)
      val trimmedLength = lineEndIdx - lineStartIdx

      val matchStartIdx = result.matchStartIndex - 1
      val matchEndIdx = matchStartIdx + matchLength

      val (lsi, lei, msi, mei) =
        if (maxLineLength > 0 && trimmedLength > maxLineLength) {
          getLineIndices(matchStartIdx, matchEndIdx, 0, matchLength, trimmedLength, maxLineLength)
        } else {
          (lineStartIdx, lineEndIdx+1, matchStartIdx, matchEndIdx)
        }

      val (prefix, lsi2) =
        if (lsi > 2) {
          ("...", lsi + 3)
        } else {
          ("", lsi)
        }
      val (suffix, lei2) =
        if (lei < (trimmedLength - 3)) {
          ("...", lei - 3)
        } else {
          ("", lei)
        }

      val formatted = prefix + line.substring(lsi2, lei2) + suffix

      if (settings.colorize) {
        colorize(formatted, msi, mei, settings.lineColor)
      } else {
        formatted
      }
    }
  }

  private def formatResultLine(result: SearchResult): String = {
    if (result.line.isEmpty || result.line.get.trim.isEmpty || settings.maxLineLength == 0) {
      ""
    } else if (settings.maxLineLength > 0 && result.matchEndIndex - result.matchStartIndex > settings.maxLineLength) {
      formatResultMatch(result)
    } else {
      formatResultLineWithMatch(result)
    }
  }

  def singleLineFormat(result: SearchResult): String = {
    val filepath = if (result.file.isDefined) fileResultFormatter.formatFileResult(result.file.get) else "<text>"
    val matchString =
      if (result.lineNum > 0) {
        ": %d: [%d:%d]: %s".format(result.lineNum, result.matchStartIndex, result.matchEndIndex,
          formatResultLine(result))
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
        val filepath = if (result.file.isDefined) fileResultFormatter.formatFileResult(result.file.get) else "<text>"
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
          line = colorize(line, result.matchStartIndex - 1, result.matchEndIndex - 1, settings.lineColor)
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

class SearchResultSorter(val settings: SearchSettings) {
  private def getSearchResultComparator: (SearchResult, SearchResult) => Boolean = {
    if (settings.sortDescending) {
      settings.sortBy match {
        case SortBy.FileName => (sr1: SearchResult, sr2: SearchResult) => sr2.beforeByName(sr1, settings.sortCaseInsensitive)
        case SortBy.FileSize => (sr1: SearchResult, sr2: SearchResult) => sr2.beforeBySize(sr1, settings.sortCaseInsensitive)
        case SortBy.FileType => (sr1: SearchResult, sr2: SearchResult) => sr2.beforeByType(sr1, settings.sortCaseInsensitive)
        case SortBy.LastMod => (sr1: SearchResult, sr2: SearchResult) => sr2.beforeByLastMod(sr1, settings.sortCaseInsensitive)
        case _ => (sr1: SearchResult, sr2: SearchResult) => sr2.beforeByPath(sr1, settings.sortCaseInsensitive)
      }
    } else {
      settings.sortBy match {
        case SortBy.FileName => (sr1: SearchResult, sr2: SearchResult) => sr1.beforeByName(sr2, settings.sortCaseInsensitive)
        case SortBy.FileSize => (sr1: SearchResult, sr2: SearchResult) => sr1.beforeBySize(sr2, settings.sortCaseInsensitive)
        case SortBy.FileType => (sr1: SearchResult, sr2: SearchResult) => sr1.beforeByType(sr2, settings.sortCaseInsensitive)
        case SortBy.LastMod => (sr1: SearchResult, sr2: SearchResult) => sr1.beforeByLastMod(sr2, settings.sortCaseInsensitive)
        case _ => (sr1: SearchResult, sr2: SearchResult) => sr1.beforeByPath(sr2, settings.sortCaseInsensitive)
      }
    }
  }

  def sort(searchResults: Seq[SearchResult]): Seq[SearchResult] = {
    val searchResultComparator = getSearchResultComparator
    searchResults.sortWith(searchResultComparator)
  }
}
