package javasearch;

import org.junit.Test;

import java.util.Set;
import java.util.regex.Pattern;

import static org.junit.Assert.*;

public class SearchSettingsTest {

    public SearchSettingsTest() {

    }

    @Test
    public final void testDefaultSettings() {
        SearchSettings settings = new SearchSettings();
        assertFalse(settings.getArchivesOnly());
        assertFalse(settings.getDebug());
        assertFalse(settings.getDoTiming());
        assertTrue(settings.getExcludeHidden());
        assertFalse(settings.getFirstMatch());
        assertEquals(settings.getLinesAfter(), 0);
        assertEquals(settings.getLinesBefore(), 0);
        assertFalse(settings.getListDirs());
        assertFalse(settings.getListFiles());
        assertFalse(settings.getListLines());
        assertEquals(settings.getMaxLineLength(), 150);
        assertFalse(settings.getMultiLineSearch());
        assertTrue(settings.getPrintResults());
        assertFalse(settings.getPrintUsage());
        assertFalse(settings.getPrintVersion());
        assertFalse(settings.getSearchArchives());
        assertFalse(settings.getUniqueLines());
        assertFalse(settings.getVerbose());
    }

    @Test
    public final void testAddExtensions() {
        SearchSettings settings = new SearchSettings();
        settings.addInExtension("java,scala");
        Set<String> inExtensions = settings.getInExtensions();
        assertEquals(inExtensions.size(), 2);
        assertTrue(inExtensions.contains("java"));
        assertTrue(inExtensions.contains("scala"));
    }

    @Test
    public final void testAddPattern() {
        SearchSettings settings = new SearchSettings();
        settings.addSearchPattern("Searcher");
        Set<Pattern> searchPatterns = settings.getSearchPatterns();
        assertEquals(searchPatterns.size(), 1);
    }

    @Test
    public final void testSetArchivesOnly() {
        SearchSettings settings = new SearchSettings();
        settings.setArchivesOnly(true);
        assertTrue(settings.getArchivesOnly());
        assertTrue(settings.getSearchArchives());
    }

    @Test
    public final void testSetDebug() {
        SearchSettings settings = new SearchSettings();
        settings.setDebug(true);
        assertTrue(settings.getDebug());
        assertTrue(settings.getVerbose());
    }
}
