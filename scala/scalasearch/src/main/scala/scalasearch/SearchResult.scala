package scalasearch

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
  var fileResultFormatter = new FileResultFormatter(settings.findSettings)

  private def formatLineWithColor(line: String): String = {
    var formattedLine = line
    settings.searchPatterns.flatMap(p => p.findFirstMatchIn(formattedLine)).take(1).foreach { m =>
      formattedLine = colorize(formattedLine, m.start, m.end)
    }
    formattedLine
  }

  val formatLine: String => String =
    if (settings.colorize) {
      formatLineWithColor
    } else {
      (line: String) => line
    }

  def format(result: SearchResult): String = {
    if (result.linesBefore.nonEmpty || result.linesAfter.nonEmpty) {
      multiLineFormat(result)
    } else {
      singleLineFormat(result)
    }
  }

  private def colorize(s: String, matchStartIndex: Int, matchEndIndex: Int): String = {
    fileResultFormatter.colorize(s, matchStartIndex, matchEndIndex)
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
    val filepath = if (result.file.isDefined) fileResultFormatter.formatFileResult(result.file.get) else "<text>"
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
