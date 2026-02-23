package ktsearch

import ktfind.ConsoleColor
import ktfind.FileResult
import ktfind.FileType
import org.junit.jupiter.api.Assertions.assertTrue
import java.io.File
import java.nio.file.Paths
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author cary on 7/30/16.
 */
class SearchResultTest {

    fun getSearchResult(
        patternString: String, pathString: String, fileType: FileType,
        lineNum: Int, matchStartIdx: Int, matchEndIdx: Int, line: String?
    ): SearchResult {
        return getSearchResult(
            patternString, pathString, fileType, lineNum, matchStartIdx, matchEndIdx, line,
            listOf(), listOf()
        )
    }

    fun getSearchResult(
        patternString: String, pathString: String, fileType: FileType,
        lineNum: Int, matchStartIdx: Int, matchEndIdx: Int, line: String?,
        linesBefore: List<String>, linesAfter: List<String>
    ): SearchResult {
        val pattern = Regex(patternString)
        val path = Paths.get(pathString)
        val fileResult = FileResult(path, fileType)
        return SearchResult(
            pattern, fileResult, lineNum, matchStartIdx,
            matchEndIdx, line!!, linesBefore, linesAfter
        )
    }

    @Test
    fun testSingleLineSearchResult() {
        val pattern = "Search"
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher\n"
        val searchResult = getSearchResult(pattern, path, FileType.CODE, lineNum,
            matchStartIndex, matchEndIndex, line)
        assertEquals(pattern, searchResult.searchPattern.pattern)
        assertEquals(path, searchResult.file!!.path.toString())
        assertEquals(FileType.CODE, searchResult.file.fileType)
        assertEquals(lineNum, searchResult.lineNum)
        assertEquals(matchStartIndex, searchResult.matchStartIndex)
        assertEquals(matchEndIndex, searchResult.matchEndIndex)
        assertEquals(line, searchResult.line)
        assertTrue(searchResult.linesBefore.isEmpty())
        assertTrue(searchResult.linesAfter.isEmpty())
    }

    @Test
    fun testMultiLineSearchResult() {
        val pattern = "Search"
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher\n"
        val linesBefore = listOf("namespace CsSearch", "{")
        val linesAfter = listOf("\t{", "\t\tprivate readonly FileTypes _fileTypes;")
        val searchResult = getSearchResult(pattern, path, FileType.CODE, lineNum,
            matchStartIndex, matchEndIndex, line, linesBefore, linesAfter)
        assertEquals(pattern, searchResult.searchPattern.pattern)
        assertEquals(path, searchResult.file!!.path.toString())
        assertEquals(FileType.CODE, searchResult.file.fileType)
        assertEquals(lineNum, searchResult.lineNum)
        assertEquals(matchStartIndex, searchResult.matchStartIndex)
        assertEquals(matchEndIndex, searchResult.matchEndIndex)
        assertEquals(line, searchResult.line)
        assertTrue(searchResult.linesBefore.isNotEmpty())
        assertTrue(searchResult.linesAfter.isNotEmpty())
    }

    @Test
    fun testSingleLineSearchResultFormatFormat() {
        val settings = getDefaultSettings().copy(colorize = false)
        val pattern = "Search"
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher\n"
        val searchResult = getSearchResult(pattern, path, FileType.CODE, lineNum,
                matchStartIndex, matchEndIndex, line)
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, line.trim { it <= ' ' })
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerThanMaxLineLengthSearchResultFormat() {
        val settings = getDefaultSettings().copy(colorize = false, maxLineLength = 100)
        val pattern = "maxlen"
        val path = "./maxlen.txt"
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line =
            "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val searchResult = getSearchResult(pattern, path, FileType.TEXT, lineNum,
            matchStartIndex, matchEndIndex, line)
        val expectedPath = "." + File.separator + "maxlen.txt"
        val expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testSingleLineLongerColorizeSearchResultFormat() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val pattern = "maxlen"
        val path = "./maxlen.txt"
        val lineNum = 1
        val matchStartIndex = 53
        val matchEndIndex = 59
        val line =
            "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val searchResult = getSearchResult(pattern, path, FileType.TEXT, lineNum,
            matchStartIndex, matchEndIndex, line)
        val expectedLine =
            "...89012345678901234567890123456789012345678901" +
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
    fun testMatchLengthLongerColorizeSearchResultFormat() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val pattern = "\\d+maxlen\\d+"
        val path = "./maxlen.txt"
        val lineNum = 10
        val matchStartIndex = 1
        val matchEndIndex = 110
        val line =
            "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
        val searchResult = getSearchResult(pattern, path, FileType.TEXT, lineNum,
            matchStartIndex, matchEndIndex, line, listOf(), listOf())
        val expectedLine =
            "" +
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
    fun testMatchLengthLongerColorizeSearchResultFormat2() {
        val settings = getDefaultSettings().copy(colorize = true, maxLineLength = 100)
        val pattern = "\\d+maxlen\\d+"
        val path = "./maxlen.txt"
        val lineNum = 10
        val matchStartIndex = 11
        val matchEndIndex = 120
        val line =
            "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"
        val searchResult = getSearchResult(pattern, path, FileType.TEXT, lineNum,
            matchStartIndex, matchEndIndex, line, listOf(), listOf())
        val expectedLine =
            "..." +
            ConsoleColor.GREEN +
            "3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
            ConsoleColor.RESET +
            "..."
        val expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testBinaryFileSearchResultFormat() {
        val settings = getDefaultSettings()
        val pattern = "Search"
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
        val lineNum = 0
        val matchStartIndex = 0
        val matchEndIndex = 0
        val searchResult = getSearchResult(pattern, path, FileType.BINARY, lineNum,
                matchStartIndex, matchEndIndex, "")
        val expectedOutput = String.format("%s matches at [0:0]", path)
        val output = SearchResultFormatter(settings).format(searchResult)
        assertEquals(expectedOutput, output)
    }

    @Test
    fun testMultiLineSearchResultFormat() {
        val settings = getDefaultSettings().copy(colorize = false, linesBefore = 2, linesAfter = 2)
        val pattern = "Searcher"
        val path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
        val lineNum = 10
        val matchStartIndex = 15
        val matchEndIndex = 23
        val line = "\tpublic class Searcher"
        val linesBefore = listOf("namespace CsSearch", "{")
        val linesAfter = listOf("\t{", "\t\tprivate readonly FileTypes _fileTypes;")
        val searchResult = getSearchResult(pattern, path, FileType.CODE, lineNum,
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
