package scalasearch

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import scalafind.FileType.FileType
import scalafind.{ConsoleColor, FileResult, FileType}

import java.nio.file.Paths

class SearchResultTest extends AnyFunSuite {

  def getSearchResult(pattern: String, path: String, fileType: FileType, lineNum: Int,
                      matchStartIdx: Int, matchEndIdx: Int, line: Option[String],
                      linesBefore: Seq[String]=Seq.empty[String],
                      linesAfter: Seq[String]=Seq.empty[String]): SearchResult = {
    val fileResult = FileResult(Paths.get(path), fileType)
    SearchResult(pattern.r, Some(fileResult), lineNum, matchStartIdx, matchEndIdx, line, linesBefore, linesAfter)
  }

  test("test single-line search result") {
    val pattern = "Search"
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val searchResult = getSearchResult(pattern, filePath, FileType.Code, lineNum,
      matchStartIndex, matchEndIndex, Some(line))
    assertEquals(pattern, searchResult.searchPattern.pattern.pattern())
    assertEquals(filePath, searchResult.file.get.path.toString)
    assertEquals(lineNum, searchResult.lineNum)
    assertEquals(matchStartIndex, searchResult.matchStartIndex)
    assertEquals(matchEndIndex, searchResult.matchEndIndex)
    assertEquals(line, searchResult.line.get)
    assertEquals(0, searchResult.linesBefore.length)
    assertEquals(0, searchResult.linesAfter.length)
  }

  test("test single-line search result format") {
    val settings = SearchSettings(colorize = false)
    val pattern = "Search"
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val searchResult = getSearchResult(pattern, filePath, FileType.Code, lineNum,
      matchStartIndex, matchEndIndex, Some(line))
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, line.trim)
    val output = new SearchResultFormatter(settings).format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test single-line longer than maxlinelength search result") {
    val settings = SearchSettings(colorize = false, maxLineLength = 100)
    val pattern = "maxlen"
    val filePath = "./maxlen.txt"
    val lineNum = 1
    val matchStartIndex = 53
    val matchEndIndex = 59
    val line =
      "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    val searchResult = getSearchResult(pattern, filePath, FileType.Text, lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, expectedLine)
    val output = new SearchResultFormatter(settings).format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test single-line longer colorize search result") {
    val settings = SearchSettings(colorize = true, maxLineLength = 100)
    val pattern = "maxlen"
    val filePath = "./maxlen.txt"
    val lineNum = 1
    val matchStartIndex = 53
    val matchEndIndex = 59
    val line =
      "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    val searchResult = getSearchResult(pattern, filePath, FileType.Text, lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedLine = "...89012345678901234567890123456789012345678901" +
      ConsoleColor.GREEN.toString +
      "maxlen" +
      ConsoleColor.RESET.toString +
      "89012345678901234567890123456789012345678901..."
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, expectedLine)
    val output = new SearchResultFormatter(settings).format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test match length longer colorize search result") {
    val settings = SearchSettings(colorize = true, maxLineLength = 100)
    val pattern = "\\d+maxlen\\d+"
    val filePath = "./maxlen.txt"
    val lineNum = 10
    val matchStartIndex = 1
    val matchEndIndex = 110
    val line =
      "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    val searchResult = getSearchResult(pattern, filePath, FileType.Text, lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedLine = ConsoleColor.GREEN.toString +
      "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
      ConsoleColor.RESET.toString +
      "..."
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, expectedLine)
    val output = new SearchResultFormatter(settings).format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test match length longer colorize search result #2") {
    val settings = SearchSettings(colorize = true, maxLineLength = 100)
    val pattern = "\\d+maxlen\\d+"
    val filePath = "./maxlen.txt"
    val lineNum = 10
    val matchStartIndex = 11
    val matchEndIndex = 120
    val line =
      "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"
    val searchResult = getSearchResult(pattern, filePath, FileType.Text, lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedLine = "..." +
      ConsoleColor.GREEN.toString +
      "3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
      ConsoleColor.RESET.toString +
      "..."
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, expectedLine)
    val output = new SearchResultFormatter(settings).format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test binary file search result") {
    val pattern = "Search"
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
    val lineNum = 0
    val matchStartIndex = 0
    val matchEndIndex = 0
    val line: Option[String] = None
    val searchResult = getSearchResult(pattern, filePath, FileType.Binary, lineNum,
      matchStartIndex, matchEndIndex, line)
    val expectedOutput = "%s matches at [%d:%d]".format(filePath, matchStartIndex, matchEndIndex)
    val output = new SearchResultFormatter(SearchSettings()).format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test multi-line search result") {
    val pattern = "Search"
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val linesBefore = List("namespace CsSearch\n", "{\n")
    val linesAfter = List("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n")
    val searchResult = getSearchResult(pattern, filePath, FileType.Code, lineNum,
      matchStartIndex, matchEndIndex, Some(line), linesBefore, linesAfter)
    val expectedOutput =
      """================================================================================
        |%s: %d: [%d:%d]
        |--------------------------------------------------------------------------------
        |   8 | namespace CsSearch
        |   9 | {
        |> 10 | 	public class Searcher
        |  11 | 	{
        |  12 | 		private readonly FileTypes _fileTypes;
        |""".stripMargin.format(filePath, lineNum, matchStartIndex, matchEndIndex)
    val output = new SearchResultFormatter(SearchSettings(colorize = false)).format(searchResult)
    assertEquals(expectedOutput, output)
  }
}
