package javasearch;

import org.json.simple.parser.ParseException;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.*;

public class SearchOptionsTest {

    public SearchOptionsTest() {

    }

    @Test
    public final void testSettingsFromMinimalArgs() {
        String[] args = new String[]{"-s", "Search", "."};
        try {
            SearchOptions searchOptions = new SearchOptions();
            SearchSettings settings = searchOptions.settingsFromArgs(args);
            assertFalse(settings.getArchivesOnly());
            assertFalse(settings.getDebug());
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
            fail();
        } catch (ParseException | IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromValidArgs() {
        String[] args = new String[]{"-x", "java,scala", "-s", "Search", "."};
        try {
            SearchOptions searchOptions = new SearchOptions();
            SearchSettings settings = searchOptions.settingsFromArgs(args);
            assertEquals(settings.getInExtensions().size(), 2);
            assertTrue(settings.getInExtensions().contains("java"));
            assertTrue(settings.getInExtensions().contains("scala"));
            assertEquals(settings.getSearchPatterns().size(), 1);
            assertEquals("Search", settings.getSearchPatterns().toArray()[0].toString());
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            fail();
        } catch (ParseException | IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromJson() {
        StringBuilder json = new StringBuilder("{\n")
                .append("  \"startpath\": \"~/src/xsearch/\",\n")
                .append("  \"in-ext\": [\"js\",\"ts\"],\n")
                .append("  \"out-dirpattern\": \"node_module\",\n")
                .append("  \"out-filepattern\": [\"temp\"],\n")
                .append("  \"searchpattern\": \"Searcher\",\n")
                .append("  \"linesbefore\": 2,\n")
                .append("  \"linesafter\": 2,\n")
                .append("  \"debug\": true,\n")
                .append("  \"allmatches\": false,\n")
                .append("  \"includehidden\": false,\n")
                .append("}");
        try {
            SearchOptions searchOptions = new SearchOptions();
            SearchSettings settings = new SearchSettings();
            searchOptions.settingsFromJson(json.toString(), settings);

            assertEquals("~/src/xsearch/", settings.getStartPath());

            assertEquals(settings.getInExtensions().size(), 2);
            assertTrue(settings.getInExtensions().contains("js"));
            assertTrue(settings.getInExtensions().contains("ts"));

            assertEquals(settings.getOutDirPatterns().size(), 1);
            assertEquals("node_module", settings.getOutDirPatterns().toArray()[0].toString());

            assertEquals(settings.getOutFilePatterns().size(), 1);
            assertEquals("temp", settings.getOutFilePatterns().toArray()[0].toString());

            assertEquals(settings.getSearchPatterns().size(), 1);
            assertEquals("Searcher", settings.getSearchPatterns().toArray()[0].toString());

            assertEquals(settings.getLinesBefore(), 2);
            assertEquals(settings.getLinesAfter(), 2);

            assertTrue(settings.getDebug());
            assertTrue(settings.getFirstMatch());
            assertTrue(settings.getExcludeHidden());
        } catch (ParseException e) {
            System.out.println("ParseException: " + e.getMessage());
            fail();
        } catch (IOException e) {
            System.out.println("IOException: " + e.getMessage());
            fail();
        }
    }
}
