package ktsearch

import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author cary on 7/30/16.
 */
class SearcherTest {
    private fun getSettings(): SearchSettings {
        return getDefaultSettings().copy(paths=setOf("."), searchPatterns=setOf(Regex("Searcher")))
    }

    private val testFilePath = "/testFile2.txt"

    /***************************************************************************
     * searchStringIterator tests
     **************************************************************************/
    @Test
    fun testSearchStringIterator() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val lineIterator = javaClass.getResourceAsStream(testFilePath).reader().readLines().iterator()

        val results = searcher.searchLineIterator(lineIterator)

        assertEquals(2, results.size.toLong())

        val firstResult = results[0]
        val expectedFirstLineNum = 30
        assertEquals(expectedFirstLineNum, firstResult.lineNum)
        val expectedFirstMatchStartIndex = 3
        assertEquals(expectedFirstMatchStartIndex, firstResult.matchStartIndex)
        val expectedFirstMatchEndIndex = 11
        assertEquals(expectedFirstMatchEndIndex, firstResult.matchEndIndex)

        val secondResult = results[1]
        val expectedSecondLineNum = 36
        assertEquals(expectedSecondLineNum, secondResult.lineNum)
        val expectedSecondMatchStartIndex = 24
        assertEquals(expectedSecondMatchStartIndex, secondResult.matchStartIndex)
        val expectedSecondMatchEndIndex = 32
        assertEquals(expectedSecondMatchEndIndex, secondResult.matchEndIndex)
    }

    /***************************************************************************
     * searchMultiLineString tests
     **************************************************************************/
    @Test
    fun testSearchMultiLineString() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val contents = javaClass.getResourceAsStream(testFilePath).reader().readText()
        val results = searcher.searchMultilineString(contents)

        assertEquals(2, results.size.toLong())

        val firstResult = results[0]
        val expectedFirstLineNum = 30
        assertEquals(expectedFirstLineNum, firstResult.lineNum)
        val expectedFirstMatchStartIndex = 3
        assertEquals(expectedFirstMatchStartIndex, firstResult.matchStartIndex)
        val expectedFirstMatchEndIndex = 11
        assertEquals(expectedFirstMatchEndIndex, firstResult.matchEndIndex)

        val secondResult = results[1]
        val expectedSecondLineNum = 36
        assertEquals(expectedSecondLineNum, secondResult.lineNum)
        val expectedSecondMatchStartIndex = 24
        assertEquals(expectedSecondMatchStartIndex, secondResult.matchStartIndex)
        val expectedSecondMatchEndIndex = 32
        assertEquals(expectedSecondMatchEndIndex, secondResult.matchEndIndex)
    }

    @Test
    fun testSearchMultiLineStringWithLinesBefore() {
        val settings = getSettings().copy(linesBefore = 2)
        val searcher = Searcher(settings)
        val contents = javaClass.getResourceAsStream(testFilePath).reader().readText()
        val results = searcher.searchMultilineString(contents)

        assertEquals(2, results.size.toLong())

        val firstResult = results[0]
        val expectedFirstLineNum = 30
        assertEquals(expectedFirstLineNum, firstResult.lineNum)
        val expectedFirstMatchStartIndex = 3
        assertEquals(expectedFirstMatchStartIndex, firstResult.matchStartIndex)
        val expectedFirstMatchEndIndex = 11
        assertEquals(expectedFirstMatchEndIndex, firstResult.matchEndIndex)

        val secondResult = results[1]
        val expectedSecondLineNum = 36
        assertEquals(expectedSecondLineNum, secondResult.lineNum)
        val expectedSecondMatchStartIndex = 24
        assertEquals(expectedSecondMatchStartIndex, secondResult.matchStartIndex)
        val expectedSecondMatchEndIndex = 32
        assertEquals(expectedSecondMatchEndIndex, secondResult.matchEndIndex)
    }
}
