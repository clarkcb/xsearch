package ktsearch

import ktfind.ConsoleColor
import ktfind.FileResult
import ktfind.FileType
import java.io.File
import java.nio.file.Paths
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author cary on 7/30/16.
 */
class SearchResultTest {

    @Test
    fun testSingleLineSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false)
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
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerThanMaxLineLengthSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false, maxLineLength = 100)
        val pattern = Regex("maxlen")
        val path = "./maxlen.txt"
        val fileResult = FileResult(Paths.get(path), FileType.TEXT)
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val searchResult = SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
                line, listOf(), listOf())
        val expectedPath = "." + File.separator + "maxlen.txt"
        val expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerColorizeSearchResult() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val pattern = Regex("maxlen")
        val path = "./maxlen.txt"
        val fileResult = FileResult(Paths.get(path), FileType.TEXT)
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val searchResult = SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
                line, listOf(), listOf())
        val expectedLine = "...89012345678901234567890123456789012345678901" +
                ConsoleColor.GREEN +
                "maxlen" +
                ConsoleColor.RESET +
                "89012345678901234567890123456789012345678901..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testMatchLengthLongerColorizeSearchResult() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val pattern = Regex("\\d+maxlen\\d+")
        val path = "./maxlen.txt"
        val fileResult = FileResult(Paths.get(path), FileType.TEXT)
        val lineNum = 10
        val matchStartIndex = 1
        val matchEndIndex = 110
        val line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val searchResult = SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
                line, listOf(), listOf())
        val expectedLine = "" +
                ConsoleColor.GREEN +
                "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
                ConsoleColor.RESET +
                "..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testMatchLengthLongerColorizeSearchResult2() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val pattern = Regex("\\d+maxlen\\d+")
        val path = "./maxlen.txt"
        val fileResult = FileResult(Paths.get(path), FileType.TEXT)
        val lineNum = 10
        val matchStartIndex = 11
        val matchEndIndex = 120
        val line = "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"
        val searchResult = SearchResult(pattern, fileResult, lineNum, matchStartIndex, matchEndIndex,
                line, listOf(), listOf())
        val expectedLine = "..." +
                ConsoleColor.GREEN +
                "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123" +
                ConsoleColor.RESET +
                "..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testBinaryFileSearchResult() {
        val settings = getDefaultSettings()
        val pattern = Regex("Search")
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        val fileResult = FileResult(Paths.get(path), FileType.BINARY)
        val lineNum = 0
        val matchStartIndex = 0
        val matchEndIndex = 0
        val searchResult = SearchResult(pattern, fileResult, lineNum,
                matchStartIndex, matchEndIndex, "")
        val expectedOutput = String.format("%s matches at [0:0]", path)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testMultiLineSearchResult() {
        val settings = getDefaultSettings().copy(colorize = false, linesBefore = 2, linesAfter = 2)
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
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }
}
