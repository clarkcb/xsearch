package ktsearch

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

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
        assertFalse(settings.firstMatch)
        assertFalse(settings.includeHidden)
        assertEquals(0, settings.linesAfter)
        assertEquals(0, settings.linesBefore)
        assertFalse(settings.printDirs)
        assertFalse(settings.printFiles)
        assertFalse(settings.printLines)
        assertEquals(150, settings.maxLineLength)
        assertFalse(settings.multiLineSearch)
        assertEquals(1, settings.paths.size)
        assertEquals(".", settings.paths.first())
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
        assertEquals(2, settings.inExtensions.size)
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
        assertEquals(1, settings.searchPatterns.size)
        assertEquals("Search", settings.searchPatterns.first().toString())
        assertEquals(1, settings.paths.size)
        assertEquals(".", settings.paths.first())
    }

    @Test
    fun testSettingsFromJson() {
        val json = """{
                 |  "path": "~/src/xsearch/",
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

        assertEquals(1, settings.paths.size)
        assertEquals("~/src/xsearch/", settings.paths.first())

        assertEquals(2, settings.inExtensions.size)
        assertTrue(settings.inExtensions.contains("js"))
        assertTrue(settings.inExtensions.contains("ts"))

        assertEquals(4, settings.outDirPatterns.size)
        assertEquals(1, settings.outDirPatterns.count {it.pattern == "node_module"})

        assertEquals(2, settings.outFilePatterns.size)
        assertEquals(1, settings.outFilePatterns.count {it.pattern == "gulpfile"})

        assertEquals(1, settings.searchPatterns.size)
        assertEquals("Searcher", settings.searchPatterns.first().pattern)

        assertEquals(2, settings.linesBefore)
        assertEquals(2, settings.linesAfter)

        assertTrue(settings.debug)
        assertTrue(settings.firstMatch)
        assertFalse(settings.includeHidden)
    }
}
