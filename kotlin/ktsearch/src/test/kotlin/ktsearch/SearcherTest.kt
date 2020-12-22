package ktsearch

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import java.io.File

/**
 * @author cary on 7/30/16.
 */
class SearcherTest {
    private fun getSettings(): SearchSettings {
        return getDefaultSettings().copy(startPath=".", searchPatterns=setOf(Regex("Searcher")))
    }

    private val testFilePath = "/testFile2.txt"

    /***************************************************************************
     * filterFile tests
     **************************************************************************/
    @Test
    fun testFilterFile_IsHidden_False() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val file = File(".gitignore")
        assertEquals(null, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_IsHiddenIncludeHidden_True() {
        val settings = getSettings().copy(excludeHidden = false)
        val searcher = Searcher(settings)
        val file = File(".gitignore")
        val searchFile = SearchFile(file, FileType.TEXT)
        assertEquals(searchFile, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_ArchiveNoSearchArchives_False() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val file = File("archive.zip")
        assertEquals(null, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_ArchiveSearchArchives_True() {
        val settings = getSettings().copy(searchArchives = true)
        val searcher = Searcher(settings)
        val file = File("archive.zip")
        val searchFile = SearchFile(file, FileType.ARCHIVE)
        assertEquals(searchFile, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_IsArchiveSearchFile_True() {
        val settings = getSettings().copy(searchArchives = true, inArchiveExtensions = setOf("zip"))
        val searcher = Searcher(settings)
        val file = File("archive.zip")
        val searchFile = SearchFile(file, FileType.ARCHIVE)
        assertEquals(searchFile, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_NotIsArchiveSearchFile_False() {
        val settings = getSettings().copy(outExtensions = setOf("zip"))
        val searcher = Searcher(settings)
        val file = File("archive.zip")
        assertEquals(null, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_ArchiveFileArchivesOnly_True() {
        val settings = getSettings().copy(archivesOnly = true)
        val searcher = Searcher(settings)
        val file = File("archive.zip")
        val searchFile = SearchFile(file, FileType.ARCHIVE)
        assertEquals(searchFile, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val file = File("FileUtil.cs")
        val searchFile = SearchFile(file, FileType.TEXT)
        assertEquals(searchFile, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_IsSearchFile_True() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val searcher = Searcher(settings)
        val file = File("FileUtil.cs")
        val searchFile = SearchFile(file, FileType.TEXT)
        assertEquals(searchFile, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_NotIsSearchFile_False() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val searcher = Searcher(settings)
        val file = File("FileUtil.cs")
        assertEquals(null, searcher.filterToSearchFile(file))
    }

    @Test
    fun testFilterFile_NonArchiveFileArchivesOnly_False() {
        val settings = getSettings().copy(archivesOnly = true)
        val searcher = Searcher(settings)
        val file = File("FileUtil.cs")
        assertEquals(null, searcher.filterToSearchFile(file))
    }

    /***************************************************************************
     * isSearchDir tests
     **************************************************************************/
    @Test
    fun testisSearchDir_SingleDot_True() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        assertTrue(searcher.isSearchDir(File(".")))
    }

    @Test
    fun testisSearchDir_DoubleDot_True() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        assertTrue(searcher.isSearchDir(File("..")))
    }

    @Test
    fun testisSearchDir_IsHidden_False() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        assertFalse(searcher.isSearchDir(File(".git")))
    }

    @Test
    fun testisSearchDir_IsHiddenIncludeHidden_True() {
        val settings = getSettings().copy(excludeHidden = false)
        val searcher = Searcher(settings)
        assertTrue(searcher.isSearchDir(File(".git")))
    }

    @Test
    fun testisSearchDir_NoPatterns_True() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        assertTrue(searcher.isSearchDir(File("/Users")))
    }

    @Test
    fun testisSearchDir_MatchesInPattern_True() {
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("Search")))
        val searcher = Searcher(settings)
        assertTrue(searcher.isSearchDir(File("CsSearch")))
    }

    @Test
    fun testisSearchDir_MatchesOutPattern_False() {
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("Search")))
        val searcher = Searcher(settings)
        assertFalse(searcher.isSearchDir(File("CsSearch")))
    }

    @Test
    fun testisSearchDir_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inDirPatterns = setOf(Regex("SearchFiles")))
        val searcher = Searcher(settings)
        assertFalse(searcher.isSearchDir(File("CsSearch")))
    }

    @Test
    fun testisSearchDir_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outDirPatterns = setOf(Regex("SearchFiles")))
        val searcher = Searcher(settings)
        val dir = File("CsSearch")
        assertTrue(searcher.isSearchDir(dir))
    }

    /***************************************************************************
     * isSearchFile tests
     **************************************************************************/
    @Test
    fun testIsSearchFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertTrue(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_MatchesInExtension_True() {
        val settings = getSettings().copy(inExtensions = setOf("cs"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertTrue(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inExtensions = setOf("java"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertFalse(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_MatchesOutExtension_False() {
        val settings = getSettings().copy(outExtensions = setOf("cs"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertFalse(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outExtensions = setOf("java"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertTrue(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_MatchesInPattern_True() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Search")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("Searcher.cs"), FileType.TEXT)
        assertTrue(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inFilePatterns = setOf(Regex("Search")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertFalse(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_MatchesOutPattern_False() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Search")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("Searcher.cs"), FileType.TEXT)
        assertFalse(searcher.isSearchFile(file))
    }

    @Test
    fun testIsSearchFile_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outFilePatterns = setOf(Regex("Search")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("FileUtil.cs"), FileType.TEXT)
        assertTrue(searcher.isSearchFile(file))
    }

    /***************************************************************************
     * isArchiveSearchFile tests
     **************************************************************************/
    @Test
    fun testIsArchiveSearchFile_NoExtensionsNoPatterns_True() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_MatchesInExtension_True() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("zip"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_DoesNotMatchInExtension_False() {
        val settings = getSettings().copy(inArchiveExtensions = setOf("gz"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_MatchesOutExtension_False() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("zip"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_DoesNotMatchOutExtension_True() {
        val settings = getSettings().copy(outArchiveExtensions = setOf("gz"))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_MatchesInPattern_True() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("arch")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_DoesNotMatchInPattern_False() {
        val settings = getSettings().copy(inArchiveFilePatterns = setOf(Regex("archives")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_MatchesOutPattern_False() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("arch")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertFalse(searcher.isArchiveSearchFile(file))
    }

    @Test
    fun testIsArchiveSearchFile_DoesNotMatchOutPattern_True() {
        val settings = getSettings().copy(outArchiveFilePatterns = setOf(Regex("archives")))
        val searcher = Searcher(settings)
        val file = SearchFile(File("archive.zip"), FileType.ARCHIVE)
        assertTrue(searcher.isArchiveSearchFile(file))
    }

    /***************************************************************************
     * searchStringIterator tests
     **************************************************************************/
    @Test
    fun testSearchStringIterator() {
        val settings = getSettings()
        val searcher = Searcher(settings)
        val lineIterator = javaClass.getResourceAsStream(testFilePath).reader().readLines().iterator()

        val results = searcher.searchStringIterator(lineIterator)

        assertEquals(results.size.toLong(), 2)

        val firstResult = results[0]
        val expectedFirstLineNum = 29
        assertEquals(firstResult.lineNum, expectedFirstLineNum)
        val expectedFirstMatchStartIndex = 3
        assertEquals(firstResult.matchStartIndex, expectedFirstMatchStartIndex)
        val expectedFirstMatchEndIndex = 11
        assertEquals(firstResult.matchEndIndex, expectedFirstMatchEndIndex)

        val secondResult = results[1]
        val expectedSecondLineNum = 35
        assertEquals(secondResult.lineNum, expectedSecondLineNum)
        val expectedSecondMatchStartIndex = 24
        assertEquals(secondResult.matchStartIndex, expectedSecondMatchStartIndex)
        val expectedSecondMatchEndIndex = 32
        assertEquals(secondResult.matchEndIndex, expectedSecondMatchEndIndex)
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

        assertEquals(results.size.toLong(), 2)

        val firstResult = results[0]
        val expectedFirstLineNum = 29
        assertEquals(firstResult.lineNum, expectedFirstLineNum)
        val expectedFirstMatchStartIndex = 3
        assertEquals(firstResult.matchStartIndex, expectedFirstMatchStartIndex)
        val expectedFirstMatchEndIndex = 11
        assertEquals(firstResult.matchEndIndex, expectedFirstMatchEndIndex)

        val secondResult = results[1]
        val expectedSecondLineNum = 35
        assertEquals(secondResult.lineNum, expectedSecondLineNum)
        val expectedSecondMatchStartIndex = 24
        assertEquals(secondResult.matchStartIndex, expectedSecondMatchStartIndex)
        val expectedSecondMatchEndIndex = 32
        assertEquals(secondResult.matchEndIndex, expectedSecondMatchEndIndex)
    }

    @Test
    fun testSearchMultiLineStringWithLinesBefore() {
        val settings = getSettings().copy(linesBefore = 2)
        val searcher = Searcher(settings)
        val contents = javaClass.getResourceAsStream(testFilePath).reader().readText()
        val results = searcher.searchMultilineString(contents)

        assertEquals(results.size.toLong(), 2)

        val firstResult = results[0]
        val expectedFirstLineNum = 29
        assertEquals(firstResult.lineNum, expectedFirstLineNum)
        val expectedFirstMatchStartIndex = 3
        assertEquals(firstResult.matchStartIndex, expectedFirstMatchStartIndex)
        val expectedFirstMatchEndIndex = 11
        assertEquals(firstResult.matchEndIndex, expectedFirstMatchEndIndex)

        val secondResult = results[1]
        val expectedSecondLineNum = 35
        assertEquals(secondResult.lineNum, expectedSecondLineNum)
        val expectedSecondMatchStartIndex = 24
        assertEquals(secondResult.matchStartIndex, expectedSecondMatchStartIndex)
        val expectedSecondMatchEndIndex = 32
        assertEquals(secondResult.matchEndIndex, expectedSecondMatchEndIndex)
    }
}
