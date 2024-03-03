package javasearch;

import org.junit.Test;

import java.util.Set;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class SearchSettingsTest {

    public SearchSettingsTest() {

    }

    @Test
    public final void testDefaultSearchSettings() {
        SearchSettings settings = new SearchSettings();
        assertEquals(DefaultSearchSettings.COLORIZE, settings.getColorize());
        assertEquals(DefaultSearchSettings.FIRST_MATCH, settings.getFirstMatch());
        assertEquals(DefaultSearchSettings.LINES_AFTER, settings.getLinesAfter());
        assertEquals(DefaultSearchSettings.LINES_BEFORE, settings.getLinesBefore());
        assertEquals(DefaultSearchSettings.MAX_LINE_LENGTH, settings.getMaxLineLength());
        assertEquals(DefaultSearchSettings.MULTI_LINE_SEARCH, settings.getMultiLineSearch());
        assertEquals(DefaultSearchSettings.PRINT_LINES, settings.getPrintLines());
        assertEquals(DefaultSearchSettings.PRINT_RESULTS, settings.getPrintResults());
        assertEquals(DefaultSearchSettings.SEARCH_ARCHIVES, settings.getSearchArchives());
        assertEquals(DefaultSearchSettings.UNIQUE_LINES, settings.getUniqueLines());
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
