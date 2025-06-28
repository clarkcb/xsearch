package ktsearch

import ktfind.FileResult
import ktfind.FileType
import java.io.File
import java.util.*
import kotlin.test.Test
import kotlin.test.assertEquals
import java.nio.file.Paths

/**
 * @author cary on 7/30/16.
 */
class SearchResultTest {

    @Test
    fun testSingleLineSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false)
        val formatter = SearchResultFormatter(settings)
        val pattern = Regex("Search")
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val fileResult = FileResult(Paths.get(path), FileType.CODE)
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher\n"
        val searchResult = SearchResult(pattern, fileResult, lineNum,
                matchStartIndex, matchEndIndex, line)
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, line.trim { it <= ' ' })
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerThanMaxLineLengthSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false, maxLineLength = 100)
        val formatter = SearchResultFormatter(settings)
        val pattern = Regex("maxlen")
        val path = "./maxlen.txt"
        val fileResult = FileResult(Paths.get(path), FileType.TEXT)
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val linesBeforeAfter: List<String> = ArrayList()
        val searchResult = SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
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
        val path = "./maxlen.txt"
        val fileResult = FileResult(Paths.get(path), FileType.TEXT)
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val linesBeforeAfter: List<String> = ArrayList()
        val searchResult = SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
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
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        val fileResult = FileResult(Paths.get(path), FileType.BINARY)
        val lineNum = 0
        val matchStartIndex = 0
        val matchEndIndex = 0
        val searchResult = SearchResult(pattern, fileResult, lineNum,
                matchStartIndex, matchEndIndex, "")
        val expectedPath = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        val expectedOutput = String.format("%s matches at [0:0]", expectedPath)
        val output = formatter.format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testMultiLineSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false, linesBefore = 2, linesAfter = 2)
        val formatter = SearchResultFormatter(settings)
        val pattern = Regex("Searcher")
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val fileResult = FileResult(Paths.get(path), FileType.CODE)
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher"
        val linesBefore = listOf("namespace CsSearch", "{")
        val linesAfter = listOf("\t{", "\t\tprivate readonly FileTypes _fileTypes;")
        val searchResult = SearchResult(pattern, fileResult, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter)
        val expectedOutput =
                """================================================================================
                   |$path: $lineNum: [$matchStartIndex:$matchEndIndex]
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
