package ktsearch

import org.junit.Test
import org.junit.Assert.assertEquals
import java.io.File

/**
 * @author cary on 7/30/16.
 */
class SearchResultTest {

    @Test
    fun testSingleLineSearchResult() {
        val pattern = Regex("Search")
        val file = File("~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs")
        val searchFile = SearchFile(file, FileType.TEXT)
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher\n"
        val searchResult = SearchResult(pattern, searchFile, lineNum,
                matchStartIndex, matchEndIndex, line)
        val expectedPath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, line.trim { it <= ' ' })
        assertEquals(searchResult.toString(), expectedOutput)
    }

    @Test
    fun testBinaryFileSearchResult() {
        val pattern = Regex("Search")
        val file = File("~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe")
        val searchFile = SearchFile(file, FileType.BINARY)
        val lineNum = 0
        val matchStartIndex = 0
        val matchEndIndex = 0
        val searchResult = SearchResult(pattern, searchFile, lineNum,
                matchStartIndex, matchEndIndex, "")
        val expectedPath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        val expectedOutput = String.format("%s matches at [0:0]", expectedPath)
        assertEquals(searchResult.toString(), expectedOutput)
    }

    @Test
    fun testMultiLineSearchResult() {
        val pattern = Regex("Search")
        val file = File("~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs")
        val searchFile = SearchFile(file, FileType.TEXT)
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher"
        val linesBefore = listOf("namespace CsSearch", "{")
        val linesAfter = listOf("\t{", "\t\tprivate readonly FileTypes _fileTypes;")
        val searchResult = SearchResult(pattern, searchFile, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter)
        val expectedPath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val expectedOutput =
                """================================================================================
                   |$expectedPath: $lineNum: [$matchStartIndex:$matchEndIndex]
                   |--------------------------------------------------------------------------------
                   |   8 | namespace CsSearch
                   |   9 | {
                   |> 10 | 	public class Searcher
                   |  11 | 	{
                   |  12 | 		private readonly FileTypes _fileTypes;
                   |""".trimMargin()
        assertEquals(searchResult.toString(), expectedOutput)
    }
}
