package scalasearch

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class SearchResultTest extends AnyFunSuite {
  test("test single-line search result") {
    val settings = SearchSettings(colorize = false)
    val formatter = new SearchResultFormatter(settings)
    val pattern = "Search".r
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val searchFile = new SearchFile(new File(filePath), FileType.Code)
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val searchResult = SearchResult(pattern, Some(searchFile), lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, line.trim)
    val output = formatter.format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test single-line longer than maxlinelength search result") {
    val settings = SearchSettings(colorize = false, maxLineLength = 100)
    val formatter = new SearchResultFormatter(settings)
    val pattern = "maxlen".r
    val filePath = "./maxlen.txt"
    val searchFile = new SearchFile(new File(filePath), FileType.Text)
    val lineNum = 1
    val matchStartIndex = 53
    val matchEndIndex = 59
    val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    val searchResult = SearchResult(pattern, Some(searchFile), lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, expectedLine)
    val output = formatter.format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test single-line longer colorize search result") {
    val settings = SearchSettings(colorize = true, maxLineLength = 100)
    val formatter = new SearchResultFormatter(settings)
    val pattern = "maxlen".r
    val filePath = "./maxlen.txt"
    val searchFile = new SearchFile(new File(filePath), FileType.Text)
    val lineNum = 1
    val matchStartIndex = 53
    val matchEndIndex = 59
    val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    val searchResult = SearchResult(pattern, Some(searchFile), lineNum,
      matchStartIndex, matchEndIndex, Some(line), List.empty, List.empty)
    val expectedLine = "...89012345678901234567890123456789012345678901" +
      Color.GREEN +
      "maxlen" +
      Color.RESET +
      "89012345678901234567890123456789012345678901..."
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(filePath,
      lineNum, matchStartIndex, matchEndIndex, expectedLine)
    val output = formatter.format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test binary file search result") {
    val formatter = new SearchResultFormatter(SearchSettings())
    val pattern = "Search".r
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
    val searchFile = new SearchFile(new File(filePath), FileType.Binary)
    val lineNum = 0
    val matchStartIndex = 0
    val matchEndIndex = 0
    val line: Option[String] = None
    val searchResult = new SearchResult(pattern, Some(searchFile), lineNum,
      matchStartIndex, matchEndIndex, line)
    val expectedOutput = "%s matches at [%d:%d]".format(filePath, matchStartIndex, matchEndIndex)
    val output = formatter.format(searchResult)
    assertEquals(expectedOutput, output)
  }

  test("test multi-line search result") {
    val formatter = new SearchResultFormatter(SearchSettings(colorize = false))
    val pattern = "Search".r
    val filePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val searchFile = new SearchFile(new File(filePath), FileType.Code)
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val linesBefore = List("namespace CsSearch\n", "{\n")
    val linesAfter = List("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n")
    val searchResult = SearchResult(pattern, Some(searchFile), lineNum,
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
    val output = formatter.format(searchResult)
    assertEquals(expectedOutput, output)
  }
}
