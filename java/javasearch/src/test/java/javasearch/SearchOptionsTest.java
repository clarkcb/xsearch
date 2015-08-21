package javasearch;

import org.junit.Test;
import org.xml.sax.SAXException;
import javax.xml.parsers.ParserConfigurationException;
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
            assertTrue(false);
        } catch (ParserConfigurationException | SAXException | IOException e) {
            System.out.println("Exception: " + e.getMessage());
            assertTrue(false);
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
            assertTrue(settings.getSearchPatterns().toArray()[0].toString().equals("Search"));
        } catch (SearchException e) {
            System.out.println("SearchException: " + e.getMessage());
            assertTrue(false);
        } catch (ParserConfigurationException | SAXException | IOException e) {
            System.out.println("Exception: " + e.getMessage());
            assertTrue(false);
        }
    }
}
