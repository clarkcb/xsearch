package ktsearch

import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File
import java.util.*

/**
 * @author cary on 7/30/16.
 */
class SearchResultTest {

    @Test
    fun testSingleLineSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false)
        val formatter = SearchResultFormatter(settings)
        val pattern = Regex("Search")
        val file = File("~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs")
        val searchFile = SearchFile(file, FileType.CODE)
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher\n"
        val searchResult = SearchResult(pattern, searchFile, lineNum,
                matchStartIndex, matchEndIndex, line)
        val expectedPath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, line.trim { it <= ' ' })
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerThanMaxLineLengthSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false, maxLineLength = 100)
        val formatter = SearchResultFormatter(settings)
        val pattern = Regex("maxlen")
        val file = File("./maxlen.txt")
        val searchFile = SearchFile(file, FileType.TEXT)
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val linesBeforeAfter: List<String> = ArrayList()
        val searchResult = SearchResult(pattern, searchFile, lineNum, matchStartIndex, matchEndIndex,
                line, linesBeforeAfter, linesBeforeAfter)
        val expectedPath = "." + File.separator + "maxlen.txt"
        val expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerColorizeSearchResult() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val formatter = SearchResultFormatter(settings)
        val pattern = Regex("maxlen")
        val file = File("./maxlen.txt")
        val searchFile = SearchFile(file, FileType.TEXT)
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val linesBeforeAfter: List<String> = ArrayList()
        val searchResult = SearchResult(pattern, searchFile, lineNum, matchStartIndex, matchEndIndex,
                line, linesBeforeAfter, linesBeforeAfter)
        val expectedPath = "." + File.separator + "maxlen.txt"
        val expectedLine = "...89012345678901234567890123456789012345678901" +
                Color.GREEN +
                "maxlen" +
                Color.RESET +
                "89012345678901234567890123456789012345678901..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testBinaryFileSearchResult() {
        val settings = getDefaultSettings()
        val formatter = SearchResultFormatter(settings)
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
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testMultiLineSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false)
        val formatter = SearchResultFormatter(settings)
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
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }
}
