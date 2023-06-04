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
        assertEquals(DefaultSearchSettings.FIRSTMATCH, settings.getFirstMatch());
        assertEquals(DefaultSearchSettings.LINESAFTER, settings.getLinesAfter());
        assertEquals(DefaultSearchSettings.LINESBEFORE, settings.getLinesBefore());
        assertEquals(DefaultSearchSettings.LISTLINES, settings.getListLines());
        assertEquals(DefaultSearchSettings.MAXLINELENGTH, settings.getMaxLineLength());
        assertEquals(DefaultSearchSettings.MULTILINESEARCH, settings.getMultiLineSearch());
        assertEquals(DefaultSearchSettings.PRINTRESULTS, settings.getPrintResults());
        assertEquals(DefaultSearchSettings.SEARCHARCHIVES, settings.getSearchArchives());
        assertEquals(DefaultSearchSettings.UNIQUELINES, settings.getUniqueLines());
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
