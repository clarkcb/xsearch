package ktsearch

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * @author cary on 7/30/16.
 */
class SearchSettingsTest {
    @Test
    fun testDefaultSettings() {
        val settings = getDefaultSettings()
        assertEquals(false, settings.archivesOnly)
        assertEquals(true, settings.colorize)
        assertEquals(false, settings.debug)
        assertEquals(true, settings.excludeHidden)
        assertEquals(false, settings.firstMatch)
        assertEquals(0, settings.linesAfter)
        assertEquals(0, settings.linesBefore)
        assertEquals(false, settings.listDirs)
        assertEquals(false, settings.listFiles)
        assertEquals(false, settings.listLines)
        assertEquals(150, settings.maxLineLength)
        assertEquals(false, settings.multiLineSearch)
        assertEquals(false, settings.printResults)
        assertEquals(false, settings.printUsage)
        assertEquals(false, settings.printVersion)
        assertEquals(false, settings.searchArchives)
        assertEquals(true, settings.recursive)
        assertEquals(false, settings.uniqueLines)
        assertEquals(false, settings.verbose)
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

    @Test
    fun testSetArchivesOnly() {
        val settings = setArchivesOnly(getDefaultSettings(), true)
        assertTrue(settings.archivesOnly)
        assertTrue(settings.searchArchives)
    }

    @Test
    fun testSetDebug() {
        val settings = setDebug(getDefaultSettings(), true)
        assertTrue(settings.debug)
        assertTrue(settings.verbose)
    }
}
