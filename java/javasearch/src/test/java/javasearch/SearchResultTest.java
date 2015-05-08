package javasearch;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;

public class SearchResultTest {

    public SearchResultTest() {

    }

    @Test
    public final void testSingleLineSearchResult() {
        final Pattern pattern = Pattern.compile("Search");
        final SearchFile searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
            "Searcher.cs", FileType.TEXT);
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 23;
        final String line = "\tpublic class Searcher\n";
        final SearchResult searchResult = new SearchResult(pattern, searchFile, lineNum,
            matchStartIndex, matchEndIndex, line);
        final String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, line.trim());
        assertEquals(searchResult.toString(), expectedOutput);
    }

    @Test
    public final void testBinaryFileSearchResult() {
        final Pattern pattern = Pattern.compile("Search");
        final SearchFile searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
            "Searcher.exe", FileType.BINARY);
        final int lineNum = 0;
        final int matchStartIndex = 0;
        final int matchEndIndex = 0;
        final SearchResult searchResult = new SearchResult(pattern, searchFile, lineNum,
            matchStartIndex, matchEndIndex, null);
        final String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe";
        final String expectedOutput = String.format("%s matches", expectedPath);
        assertEquals(searchResult.toString(), expectedOutput);
    }

    @Test
    public final void testMultiLineSearchResult() {
        final Pattern pattern = Pattern.compile("Search");
        final SearchFile searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
                "Searcher.cs", FileType.TEXT);
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 23;
        final String line = "\tpublic class Searcher\n";
        final List<String> linesBefore = Arrays.asList("namespace CsSearch\n", "{\n");
        final List<String> linesAfter = Arrays.asList("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n");
        final SearchResult searchResult = new SearchResult(pattern, searchFile, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
        final String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        final String expectedOutput = String.format(
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
