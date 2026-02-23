package javasearch;

import javafind.ConsoleColor;
import javafind.FileResult;
import javafind.FileType;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SearchResultFormatterTest {

    public SearchResultFormatterTest() {

    }

    @Test
    public final void testSingleLineSearchResult() {
        final String pattern = "Search";
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearchLib/Searcher.cs";
        final int lineNum = 14;
        final int matchStartIndex = 14;
        final int matchEndIndex = 20;
        final String line = "public class Searcher\n";
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.CODE, lineNum,
                matchStartIndex, matchEndIndex, line);
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, line.trim());

        SearchSettings settings = new SearchSettings();
        settings.setColorize(false);
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }

    @Test
    public final void testSingleLineLongerThanMaxLineLengthSearchResult() {
        final String pattern = "maxlen";
        final String path = "./maxlen.txt";
        final int lineNum = 1;
        final int matchStartIndex = 53;
        final int matchEndIndex = 59;
        final String line =
                "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.TEXT, lineNum,
                matchStartIndex, matchEndIndex, line);
        final String expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);

        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(false);
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }

    @Test
    public final void testSingleLineLongerColorizeSearchResult() {
        final String pattern = "maxlen";
        final String path = "./maxlen.txt";
        final int lineNum = 1;
        final int matchStartIndex = 53;
        final int matchEndIndex = 59;
        final String line =
                "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.TEXT, lineNum,
                matchStartIndex, matchEndIndex, line);
        final String expectedLine = "...89012345678901234567890123456789012345678901" +
                ConsoleColor.GREEN.getValue() +
                "maxlen" +
                ConsoleColor.RESET.getValue() +
                "89012345678901234567890123456789012345678901...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);

        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }

    @Test
    public final void testMatchLengthLongerColorizeSearchResult() {
        final String pattern = "\\d+maxlen\\d+";
        final String path = "./maxlen.txt";
        final int lineNum = 1;
        final int matchStartIndex = 1;
        final int matchEndIndex = 110;
        final String line =
                "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.TEXT, lineNum,
                matchStartIndex, matchEndIndex, line);
        final String expectedLine =
                ConsoleColor.GREEN.getValue() +
                "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
                ConsoleColor.RESET.getValue() +
                "...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);

        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }

    @Test
    public final void testMatchLengthLongerColorizeSearchResult2() {
        final String pattern = "\\d+maxlen\\d+";
        final String path = "./maxlen.txt";
        final int lineNum = 1;
        final int matchStartIndex = 11;
        final int matchEndIndex = 120;
        final String line =
                "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ";
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.TEXT, lineNum,
                matchStartIndex, matchEndIndex, line);
        final String expectedLine =
                "..." +
                ConsoleColor.GREEN.getValue() +
                "3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
                ConsoleColor.RESET.getValue() +
                "...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);

        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }

    @Test
    public final void testBinaryFileSearchResult() {
        final String pattern = "Search";
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearchLib/Searcher.exe";
        final int lineNum = 0;
        final int matchStartIndex = 0;
        final int matchEndIndex = 0;
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.BINARY, lineNum,
                matchStartIndex, matchEndIndex, null);
        final String expectedOutput = String.format("%s matches at [0:0]", path);

        SearchSettings settings = new SearchSettings();
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }

    @Test
    public final void testMultiLineSearchResult() {
        final String pattern = "Search";
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearchLib/Searcher.cs";
        final int lineNum = 14;
        final int matchStartIndex = 14;
        final int matchEndIndex = 20;
        final String line = "\tpublic class Searcher\n";
        final List<String> linesBefore = Arrays.asList("namespace CsSearch\n", "{\n");
        final List<String> linesAfter = Arrays.asList("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n");
        final SearchResult searchResult = SearchResultTest.getSearchResult(pattern, path, FileType.CODE, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
        final String expectedOutput = String.format(
                "================================================================================\n" +
                "%s: %d: [%d:%d]\n" +
                "--------------------------------------------------------------------------------\n" +
                "  12 | namespace CsSearch\n" +
                "  13 | {\n" +
                "> 14 | \tpublic class Searcher\n" +
                "  15 | \t{\n" +
                "  16 | \t\tprivate readonly FileTypes _fileTypes;\n",
                path, lineNum, matchStartIndex, matchEndIndex);

        SearchSettings settings = new SearchSettings();
        settings.setColorize(false);
        var formatter = new SearchResultFormatter(settings);

        assertEquals(expectedOutput, formatter.format(searchResult));
    }
}
