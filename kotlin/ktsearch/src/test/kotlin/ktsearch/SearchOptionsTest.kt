package ktsearch

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

/**
 * @author cary on 7/30/16.
 */
class SearchOptionsTest {

    @Test
    fun testSettingsFromMinimalArgs() {
        val args = arrayOf("-s", "Search", ".")
        val searchOptions = SearchOptions()
        val settings = searchOptions.settingsFromArgs(args)
        assertFalse(settings.archivesOnly)
        assertFalse(settings.debug)
        assertTrue(settings.excludeHidden)
        assertFalse(settings.firstMatch)
        assertEquals(settings.linesAfter, 0)
        assertEquals(settings.linesBefore, 0)
        assertFalse(settings.listDirs)
        assertFalse(settings.listFiles)
        assertFalse(settings.listLines)
        assertEquals(settings.maxLineLength, 150)
        assertFalse(settings.multiLineSearch)
        assertTrue(settings.printResults)
        assertFalse(settings.printUsage)
        assertFalse(settings.printVersion)
        assertFalse(settings.searchArchives)
        assertFalse(settings.uniqueLines)
        assertFalse(settings.verbose)
    }

    @Test
    fun testSettingsFromValidArgs() {
        val args = arrayOf("-x", "java,scala", "-s", "Search", ".")
        val searchOptions = SearchOptions()
        val settings = searchOptions.settingsFromArgs(args)
        assertEquals(settings.inExtensions.size, 2)
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
        assertEquals(settings.searchPatterns.size, 1)
        assertTrue(settings.searchPatterns.first().toString().equals("Search"))
    }

    @Test
    fun testSettingsFromJson() {
        val json = """{
                 |  "startpath": "~/src/xsearch/",
                 |  "in-ext": ["js","ts"],
                 |  "out-dirpattern": ["build", "node_module", "tests", "typings"],
                 |  "out-filepattern": ["gulpfile", "\\.min\\."],
                 |  "searchpattern": "Searcher",
                 |  "linesbefore": 2,
                 |  "linesafter": 2,
                 |  "debug": true,
                 |  "allmatches": false,
                 |  "includehidden": false
                 |}""".trimMargin()
        val searchOptions = SearchOptions()
        val settings = searchOptions.settingsFromJson(json, getDefaultSettings())

        assertTrue(settings.startPath == "~/src/xsearch/")

        assertEquals(settings.inExtensions.size, 2)
        assertTrue(settings.inExtensions.contains("js"))
        assertTrue(settings.inExtensions.contains("ts"))

        assertEquals(settings.outDirPatterns.size, 4)
        assertTrue(settings.outDirPatterns.count {it.pattern == "node_module"} == 1)

        assertEquals(settings.outFilePatterns.size, 2)
        assertTrue(settings.outFilePatterns.count {it.pattern == "gulpfile"} == 1)

        assertEquals(settings.searchPatterns.size, 1)
        assertTrue(settings.searchPatterns.first().pattern == "Searcher")

        assertEquals(settings.linesBefore, 2)
        assertEquals(settings.linesAfter, 2)

        assertTrue(settings.debug)
        assertTrue(settings.firstMatch)
        assertTrue(settings.excludeHidden)
    }
}
