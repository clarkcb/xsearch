package ktsearch

import ktfind.addExtensions
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * @author cary on 7/30/16.
 */
class SearchSettingsTest {
    @Test
    fun testDefaultSettings() {
        val settings = getDefaultSettings()
        assertFalse(settings.archivesOnly)
        assertTrue(settings.colorize)
        assertFalse(settings.debug)
        assertFalse(settings.firstMatch)
        assertFalse(settings.includeHidden)
        assertEquals(0, settings.linesAfter)
        assertEquals(0, settings.linesBefore)
        assertEquals(150, settings.maxLineLength)
        assertFalse(settings.multiLineSearch)
        assertFalse(settings.printDirs)
        assertFalse(settings.printFiles)
        assertFalse(settings.printLines)
        assertFalse(settings.printResults)
        assertFalse(settings.printUsage)
        assertFalse(settings.printVersion)
        assertFalse(settings.searchArchives)
        assertTrue(settings.recursive)
        assertFalse(settings.uniqueLines)
        assertFalse(settings.verbose)
    }

    @Test
    fun testAddExtensions() {
        val settings = getDefaultSettings().copy(inExtensions = setOf("java", "scala"))
        assertEquals(2, settings.inExtensions.size.toLong())
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
    }

    @Test
    fun testAddExtensionsAsCommaString() {
        val defaultSettings = getDefaultSettings()
        val settings = defaultSettings.
                copy(inExtensions = addExtensions("java,scala", defaultSettings.inExtensions))
        assertEquals(2, settings.inExtensions.size.toLong())
        assertTrue(settings.inExtensions.contains("java"))
        assertTrue(settings.inExtensions.contains("scala"))
    }

    @Test
    fun testSetArchivesOnly() {
        val settings = setArchivesOnly(getDefaultSettings(), true)
        assertTrue(settings.archivesOnly)
        assertTrue(settings.searchArchives)
    }
}
