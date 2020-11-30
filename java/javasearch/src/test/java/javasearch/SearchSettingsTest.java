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
    public final void testDefaultSettings() {
        SearchSettings settings = new SearchSettings();
        assertEquals(DefaultSettings.ARCHIVESONLY, settings.getArchivesOnly());
        assertEquals(DefaultSettings.DEBUG, settings.getDebug());
        assertEquals(DefaultSettings.EXCLUDEHIDDEN, settings.getExcludeHidden());
        assertEquals(DefaultSettings.FIRSTMATCH, settings.getFirstMatch());
        assertEquals(DefaultSettings.LINESAFTER, settings.getLinesAfter());
        assertEquals(DefaultSettings.LINESBEFORE, settings.getLinesBefore());
        assertEquals(DefaultSettings.LISTDIRS, settings.getListDirs());
        assertEquals(DefaultSettings.LISTFILES, settings.getListFiles());
        assertEquals(DefaultSettings.LISTLINES, settings.getListLines());
        assertEquals(DefaultSettings.MAXLINELENGTH, settings.getMaxLineLength());
        assertEquals(DefaultSettings.MULTILINESEARCH, settings.getMultiLineSearch());
        assertEquals(DefaultSettings.PRINTRESULTS, settings.getPrintResults());
        assertEquals(DefaultSettings.PRINTUSAGE, settings.getPrintUsage());
        assertEquals(DefaultSettings.PRINTVERSION, settings.getPrintVersion());
        assertEquals(DefaultSettings.SEARCHARCHIVES, settings.getSearchArchives());
        assertEquals(DefaultSettings.UNIQUELINES, settings.getUniqueLines());
        assertEquals(DefaultSettings.VERBOSE, settings.getVerbose());
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
