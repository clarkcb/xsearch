package javasearch;

import org.junit.Test;

import java.util.Set;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SearchSettingsTest {

    public SearchSettingsTest() {

    }

    @Test
    public final void testDefaultSettings() {
        SearchSettings settings = new SearchSettings();
        assertEquals(settings.getArchivesOnly(), DefaultSettings.ARCHIVESONLY);
        assertEquals(settings.getDebug(), DefaultSettings.DEBUG);
        assertEquals(settings.getExcludeHidden(), DefaultSettings.EXCLUDEHIDDEN);
        assertEquals(settings.getFirstMatch(), DefaultSettings.FIRSTMATCH);
        assertEquals(settings.getLinesAfter(), DefaultSettings.LINESAFTER);
        assertEquals(settings.getLinesBefore(), DefaultSettings.LINESBEFORE);
        assertEquals(settings.getListDirs(), DefaultSettings.LISTDIRS);
        assertEquals(settings.getListFiles(), DefaultSettings.LISTFILES);
        assertEquals(settings.getListLines(), DefaultSettings.LISTLINES);
        assertEquals(settings.getMaxLineLength(), DefaultSettings.MAXLINELENGTH);
        assertEquals(settings.getMultiLineSearch(), DefaultSettings.MULTILINESEARCH);
        assertEquals(settings.getPrintResults(), DefaultSettings.PRINTRESULTS);
        assertEquals(settings.getPrintUsage(), DefaultSettings.PRINTUSAGE);
        assertEquals(settings.getPrintVersion(), DefaultSettings.PRINTVERSION);
        assertEquals(settings.getSearchArchives(), DefaultSettings.SEARCHARCHIVES);
        assertEquals(settings.getUniqueLines(), DefaultSettings.UNIQUELINES);
        assertEquals(settings.getVerbose(), DefaultSettings.VERBOSE);
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
