package scalasearch

import org.junit.Assert._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SearchResultTest extends FunSuite {
  test("test single-line search result") {
    val pattern = "Search".r
    val searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
      "Searcher.cs", FileType.Text)
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val searchResult = new SearchResult(pattern, searchFile, lineNum,
      matchStartIndex, matchEndIndex, line)
    val expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val expectedOutput = "%s: %d: [%d:%d]: %s".format(expectedPath,
      lineNum, matchStartIndex, matchEndIndex, line.trim)
    System.out.println("\nsearchResult.toString():\n" + searchResult.toString)
    System.out.println("expectedOutput:\n" + expectedOutput)
    assertEquals(searchResult.toString, expectedOutput)
  }

  test("test binary file search result") {
    val pattern = "Search".r
    val searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
      "Searcher.exe", FileType.Binary)
    val lineNum = 0
    val matchStartIndex = 0
    val matchEndIndex = 0
    val line:String = null
    val searchResult = new SearchResult(pattern, searchFile, lineNum,
      matchStartIndex, matchEndIndex, line)
    val expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
    val expectedOutput = "%s matches".format(expectedPath)
    System.out.println("\nsearchResult.toString():\n" + searchResult.toString)
    System.out.println("expectedOutput:\n" + expectedOutput)
    assertEquals(searchResult.toString, expectedOutput)
  }

  test("test multi-line search result") {
    val pattern = "Search".r
    val searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
      "Searcher.cs", FileType.Text)
    val lineNum = 10
    val matchStartIndex = 15
    val matchEndIndex = 23
    val line = "\tpublic class Searcher\n"
    val linesBefore = List("namespace CsSearch\n", "{\n")
    val linesAfter = List("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n")
    val searchResult = new SearchResult(pattern, searchFile, lineNum,
      matchStartIndex, matchEndIndex, line, linesBefore, linesAfter)
    val expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
    val expectedOutput =
      """================================================================================
        |%s: %d: [%d:%d]
        |--------------------------------------------------------------------------------
        |   8 | namespace CsSearch
        |   9 | {
        |> 10 | 	public class Searcher
        |  11 | 	{
        |  12 | 		private readonly FileTypes _fileTypes;
        |""".stripMargin.format(expectedPath, lineNum, matchStartIndex, matchEndIndex)
    System.out.println("\nsearchResult.toString():\n" + searchResult.toString)
    System.out.println("expectedOutput:\n" + expectedOutput)
    assertEquals(searchResult.toString, expectedOutput)
  }
}
