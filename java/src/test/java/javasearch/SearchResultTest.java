package javasearch;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import static org.junit.Assert.*;

public class SearchResultTest {

    public SearchResultTest() {

    }

    @Test
    public void testSingleLineSearchResult() {
        Pattern pattern = Pattern.compile("Search");
        SearchFile searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
            "Searcher.cs", FileType.TEXT);
        int lineNum = 10;
        int matchStartIndex = 15;
        int matchEndIndex = 23;
        String line = "\tpublic class Searcher\n";
        SearchResult searchResult = new SearchResult(pattern, searchFile, lineNum,
            matchStartIndex, matchEndIndex, line);
        String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        String expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, line.trim());
        assertEquals(searchResult.toString(), expectedOutput);
    }

    @Test
    public void testBinaryFileSearchResult() {
        Pattern pattern = Pattern.compile("Search");
        SearchFile searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
            "Searcher.exe", FileType.BINARY);
        int lineNum = 0;
        int matchStartIndex = 0;
        int matchEndIndex = 0;
        SearchResult searchResult = new SearchResult(pattern, searchFile, lineNum,
            matchStartIndex, matchEndIndex, null);
        String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe";
        String expectedOutput = String.format("%s matches", expectedPath);
        assertEquals(searchResult.toString(), expectedOutput);
    }

    @Test
    public void testMultiLineSearchResult() {
        Pattern pattern = Pattern.compile("Search");
        SearchFile searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
                "Searcher.cs", FileType.TEXT);
        int lineNum = 10;
        int matchStartIndex = 15;
        int matchEndIndex = 23;
        String line = "\tpublic class Searcher\n";
        List<String> linesBefore = Arrays.asList("namespace CsSearch\n", "{\n");
        List<String> linesAfter = Arrays.asList("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n");
        SearchResult searchResult = new SearchResult(pattern, searchFile, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
        String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        String expectedOutput = String.format(
                "================================================================================\n" +
                "%s: %d: [%d:%d]\n" +
                "--------------------------------------------------------------------------------\n" +
                "   8 | namespace CsSearch\n" +
                "   9 | {\n" +
                "> 10 | \tpublic class Searcher\n" +
                "  11 | \t{\n" +
                "  12 | \t\tprivate readonly FileTypes _fileTypes;\n",
                expectedPath, lineNum, matchStartIndex, matchEndIndex);
        assertEquals(searchResult.toString(), expectedOutput);
    }
}