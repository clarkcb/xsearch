package javasearch;

import javafind.FileUtil;
import org.junit.jupiter.api.Test;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class SearcherTest {

    public SearcherTest() {}

    private static SearchSettings getSettings() {
        SearchSettings settings = new SearchSettings();
        settings.addPath(".");
        settings.addSearchPattern("Searcher");
        return settings;
    }

    private static final String testFilePath = "/testFile2.txt";

    /*************************************************************
     * searchStringIterator test
     *************************************************************/
    @Test
    public final void testSearchStringIterator() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        Iterator<String> lineIterator;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            List<String> lines = FileUtil.getStreamLines(is);
            lineIterator = lines.iterator();
            List<SearchResult> results = searcher.searchStringIterator(lineIterator);

            assertEquals(results.size(), 2);

            SearchResult firstResult = results.get(0);
            int expectedFirstLineNum = 29;
            assertEquals(firstResult.getLineNum(), expectedFirstLineNum);
            int expectedFirstMatchStartIndex = 3;
            assertEquals(firstResult.getMatchStartIndex(), expectedFirstMatchStartIndex);
            int expectedFirstMatchEndIndex = 11;
            assertEquals(firstResult.getMatchEndIndex(), expectedFirstMatchEndIndex);

            SearchResult secondResult = results.get(1);
            int expectedSecondLineNum = 35;
            assertEquals(secondResult.getLineNum(), expectedSecondLineNum);
            int expectedSecondMatchStartIndex = 24;
            assertEquals(secondResult.getMatchStartIndex(), expectedSecondMatchStartIndex);
            int expectedSecondMatchEndIndex = 32;
            assertEquals(secondResult.getMatchEndIndex(), expectedSecondMatchEndIndex);

        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    /*************************************************************
     * searchMultiLineString tests
     *************************************************************/
    @Test
    public final void testSearchMultiLineString() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        String contents;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            contents = FileUtil.getStreamContents(is);
            //System.out.println("contents: " + contents);
            List<SearchResult> results = searcher.searchMultiLineString(contents);

            assert(results.size() == 2);

            SearchResult firstResult = results.get(0);
            int expectedFirstLineNum = 29;
            assertEquals(firstResult.getLineNum(), expectedFirstLineNum);
            int expectedFirstMatchStartIndex = 3;
            assertEquals(firstResult.getMatchStartIndex(), expectedFirstMatchStartIndex);
            int expectedFirstMatchEndIndex = 11;
            assertEquals(firstResult.getMatchEndIndex(), expectedFirstMatchEndIndex);

            SearchResult secondResult = results.get(1);
            int expectedSecondLineNum = 35;
            assertEquals(secondResult.getLineNum(), expectedSecondLineNum);
            int expectedSecondMatchStartIndex = 24;
            assertEquals(secondResult.getMatchStartIndex(), expectedSecondMatchStartIndex);
            int expectedSecondMatchEndIndex = 32;
            assertEquals(secondResult.getMatchEndIndex(), expectedSecondMatchEndIndex);

        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public final void testSearchMultiLineStringWithLinesBefore() {
        SearchSettings settings = getSettings();
        settings.setLinesBefore(2);
        Searcher searcher = new Searcher(settings);
        String contents;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            contents = FileUtil.getStreamContents(is);
            //System.out.println("contents: " + contents);
            List<SearchResult> results = searcher.searchMultiLineString(contents);

            assertEquals(results.size(), 2);

            SearchResult firstResult = results.get(0);
            System.out.println("firstResult:\n" + firstResult);
            int expectedFirstLineNum = 29;
            assertEquals(firstResult.getLineNum(), expectedFirstLineNum);
            int expectedFirstMatchStartIndex = 3;
            assertEquals(firstResult.getMatchStartIndex(), expectedFirstMatchStartIndex);
            int expectedFirstMatchEndIndex = 11;
            assertEquals(firstResult.getMatchEndIndex(), expectedFirstMatchEndIndex);

            SearchResult secondResult = results.get(1);
            System.out.println("secondResult:\n" + secondResult);
            int expectedSecondLineNum = 35;
            assertEquals(secondResult.getLineNum(), expectedSecondLineNum);
            int expectedSecondMatchStartIndex = 24;
            assertEquals(secondResult.getMatchStartIndex(), expectedSecondMatchStartIndex);
            int expectedSecondMatchEndIndex = 32;
            assertEquals(secondResult.getMatchEndIndex(), expectedSecondMatchEndIndex);

        } catch (IllegalArgumentException e) {
            fail();
        }
    }
}
