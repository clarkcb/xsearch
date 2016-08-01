package ktsearch

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue

/**
 * @author cary on 7/30/16.
 */
class SearchSettingsTest {
    @Test
    fun testDefaultSettings() {
        val settings = getDefaultSettings()
        assertEquals(settings.archivesOnly, false)
        assertEquals(settings.debug, false)
        assertEquals(settings.excludeHidden, false)
        assertEquals(settings.firstMatch, false)
        assertEquals(settings.linesAfter, 0)
        assertEquals(settings.linesBefore, 0)
        assertEquals(settings.listDirs, false)
        assertEquals(settings.listFiles, false)
        assertEquals(settings.listLines, false)
        assertEquals(settings.maxLineLength, 150)
        assertEquals(settings.multiLineSearch, false)
        assertEquals(settings.printResults, false)
        assertEquals(settings.printUsage, false)
        assertEquals(settings.printVersion, false)
        assertEquals(settings.searchArchives, false)
        assertEquals(settings.uniqueLines, false)
        assertEquals(settings.verbose, false)
    }

    @Test
    fun testAddExtensions() {
        val settings = getDefaultSettings().copy(inExtensions = setOf("java", "scala"))
        assertEquals(settings.inExtensions.size.toLong(), 2)
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
    }

    @Test
    fun testAddExtensionsAsCommaString() {
        val defaultSettings = getDefaultSettings()
        val settings = defaultSettings.
                copy(inExtensions = addExtensions("java,scala", defaultSettings.inExtensions))
        assertEquals(settings.inExtensions.size.toLong(), 2)
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
    }
}