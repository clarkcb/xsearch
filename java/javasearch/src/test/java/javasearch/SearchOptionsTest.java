package javasearch;

import org.junit.Test;

import static org.junit.Assert.*;

public class SearchOptionsTest {

    private SearchOptions searchOptions = new SearchOptions();

    public SearchOptionsTest() {

    }

    @Test
    public final void testSettingsFromMinimalArgs() {
        String[] args = new String[]{"-s", "Search", "."};
        try {
            SearchSettings settings = searchOptions.settingsFromArgs(args);
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
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            assertTrue(false);
        }
    }

    @Test
    public final void testSettingsFromValidArgs() {
        String[] args = new String[]{"-x", "java,scala", "-s", "Search", "."};
        try {
            SearchSettings settings = searchOptions.settingsFromArgs(args);
            assert(settings.getInExtensions().size() == 2);
            assert(settings.getInExtensions().contains("java"));
            assert(settings.getInExtensions().contains("scala"));
            assert(settings.getSearchPatterns().size() == 1);
            assert(settings.getSearchPatterns().toArray()[0].toString().equals("Search"));
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            assertTrue(false);
        }
    }
}
