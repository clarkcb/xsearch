package javasearch;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.*;

public class SearchSettingsTest {

    public SearchSettingsTest() {

    }

    @Test
    public void testDefaultSettings() {
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
}