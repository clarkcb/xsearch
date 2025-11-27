package javasearch;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

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
            assertFalse(settings.getFirstMatch());
            assertFalse(settings.getIncludeHidden());
            assertEquals(0, settings.getLinesAfter());
            assertEquals(0, settings.getLinesBefore());
            assertEquals(150, settings.getMaxLineLength());
            assertFalse(settings.getMultiLineSearch());
            assertEquals(1, settings.getPaths().size());
            assertTrue(settings.getPaths().contains(Paths.get(".")));
            assertFalse(settings.getPrintDirs());
            assertFalse(settings.getPrintFiles());
            assertFalse(settings.getPrintLines());
            assertTrue(settings.getPrintResults());
            assertFalse(settings.getPrintUsage());
            assertFalse(settings.getPrintVersion());
            assertFalse(settings.getSearchArchives());
            assertFalse(settings.getUniqueLines());
            assertFalse(settings.getVerbose());
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            fail();
        } catch (IOException e) {
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
            assertEquals(2, settings.getInExtensions().size());
            assertTrue(settings.getInExtensions().contains("java"));
            assertTrue(settings.getInExtensions().contains("scala"));
            assertEquals(1, settings.getSearchPatterns().size());
            assertEquals("Search", settings.getSearchPatterns().toArray()[0].toString());
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            fail();
        } catch (IOException e) {
            System.out.println("Exception: " + e.getMessage());
            fail();
        }
    }

    @Test
    public final void testSettingsFromJson() {
        StringBuilder json = new StringBuilder("{\n")
                .append("  \"path\": \"~/src/xsearch/\",\n")
                .append("  \"in-ext\": [\"js\",\"ts\"],\n")
                .append("  \"out-dirpattern\": \"node_module\",\n")
                .append("  \"out-filepattern\": [\"temp\"],\n")
                .append("  \"searchpattern\": \"Searcher\",\n")
                .append("  \"linesbefore\": 2,\n")
                .append("  \"linesafter\": 2,\n")
                .append("  \"debug\": true,\n")
                .append("  \"allmatches\": false,\n")
                .append("  \"includehidden\": false\n")
                .append("}");
        try {
            SearchOptions searchOptions = new SearchOptions();
            SearchSettings settings = new SearchSettings();
            searchOptions.updateSettingsFromJson(settings, json.toString());

            assertEquals(1, settings.getPaths().size());
            assertTrue(settings.getPaths().contains(Paths.get("~/src/xsearch/")));

            assertEquals(2, settings.getInExtensions().size());
            assertTrue(settings.getInExtensions().contains("js"));
            assertTrue(settings.getInExtensions().contains("ts"));

            assertEquals(1, settings.getOutDirPatterns().size());
            assertEquals("node_module", settings.getOutDirPatterns().toArray()[0].toString());

            assertEquals(1, settings.getOutFilePatterns().size());
            assertEquals("temp", settings.getOutFilePatterns().toArray()[0].toString());

            assertEquals(1, settings.getSearchPatterns().size());
            assertEquals("Searcher", settings.getSearchPatterns().toArray()[0].toString());

            assertEquals(2, settings.getLinesBefore());
            assertEquals(2, settings.getLinesAfter());

            assertTrue(settings.getDebug());
            assertTrue(settings.getFirstMatch());
            assertFalse(settings.getIncludeHidden());
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            fail();
        } catch (IOException e) {
            System.out.println("IOException: " + e.getMessage());
            fail();
        }
    }
}
