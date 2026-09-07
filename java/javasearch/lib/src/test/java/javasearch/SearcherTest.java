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
        var config = new SearchConfig();
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(config, settings);
        Iterator<String> lineIterator;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            List<String> lines = FileUtil.getStreamLines(is);
            lineIterator = lines.iterator();
            List<SearchResult> results = searcher.searchStringIterator(lineIterator);

            assertEquals(2, results.size());

            SearchResult firstResult = results.get(0);
            int expectedFirstLineNum = 30;
            assertEquals(expectedFirstLineNum, firstResult.getLineNum());
            int expectedFirstMatchStartIndex = 3;
            assertEquals(expectedFirstMatchStartIndex, firstResult.getMatchStartIndex());
            int expectedFirstMatchEndIndex = 11;
            assertEquals(expectedFirstMatchEndIndex, firstResult.getMatchEndIndex());

            SearchResult secondResult = results.get(1);
            int expectedSecondLineNum = 36;
            assertEquals(expectedSecondLineNum, secondResult.getLineNum());
            int expectedSecondMatchStartIndex = 24;
            assertEquals(expectedSecondMatchStartIndex, secondResult.getMatchStartIndex());
            int expectedSecondMatchEndIndex = 32;
            assertEquals(expectedSecondMatchEndIndex, secondResult.getMatchEndIndex());

        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    /*************************************************************
     * searchMultiLineString tests
     *************************************************************/
    @Test
    public final void testSearchMultiLineString() {
        var config = new SearchConfig();
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(config, settings);
        String contents;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            contents = FileUtil.getStreamContents(is);
            //System.out.println("contents: " + contents);
            List<SearchResult> results = searcher.searchMultiLineString(contents);

            assertEquals(2, results.size());

            SearchResult firstResult = results.get(0);
            int expectedFirstLineNum = 30;
            assertEquals(expectedFirstLineNum, firstResult.getLineNum());
            int expectedFirstMatchStartIndex = 3;
            assertEquals(expectedFirstMatchStartIndex, firstResult.getMatchStartIndex());
            int expectedFirstMatchEndIndex = 11;
            assertEquals(expectedFirstMatchEndIndex, firstResult.getMatchEndIndex());

            SearchResult secondResult = results.get(1);
            int expectedSecondLineNum = 36;
            assertEquals(expectedSecondLineNum, secondResult.getLineNum());
            int expectedSecondMatchStartIndex = 24;
            assertEquals(expectedSecondMatchStartIndex, secondResult.getMatchStartIndex());
            int expectedSecondMatchEndIndex = 32;
            assertEquals(expectedSecondMatchEndIndex, secondResult.getMatchEndIndex());

        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public final void testSearchMultiLineStringWithLinesBefore() {
        var config = new SearchConfig();
        SearchSettings settings = getSettings();
        settings.setLinesBefore(2);
        Searcher searcher = new Searcher(config, settings);
        String contents;
        try {
            InputStream is = getClass().getResourceAsStream(testFilePath);
            contents = FileUtil.getStreamContents(is);
            //System.out.println("contents: " + contents);
            List<SearchResult> results = searcher.searchMultiLineString(contents);

            assertEquals(2, results.size());

            SearchResult firstResult = results.get(0);
            System.out.println("firstResult:\n" + firstResult);
            int expectedFirstLineNum = 30;
            assertEquals(expectedFirstLineNum, firstResult.getLineNum());
            int expectedFirstMatchStartIndex = 3;
            assertEquals(expectedFirstMatchStartIndex, firstResult.getMatchStartIndex());
            int expectedFirstMatchEndIndex = 11;
            assertEquals(expectedFirstMatchEndIndex, firstResult.getMatchEndIndex());

            SearchResult secondResult = results.get(1);
            System.out.println("secondResult:\n" + secondResult);
            int expectedSecondLineNum = 36;
            assertEquals(expectedSecondLineNum, secondResult.getLineNum());
            int expectedSecondMatchStartIndex = 24;
            assertEquals(expectedSecondMatchStartIndex, secondResult.getMatchStartIndex());
            int expectedSecondMatchEndIndex = 32;
            assertEquals(expectedSecondMatchEndIndex, secondResult.getMatchEndIndex());

        } catch (IllegalArgumentException e) {
            fail();
        }
    }
}
