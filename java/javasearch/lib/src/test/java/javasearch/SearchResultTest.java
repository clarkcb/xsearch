package javasearch;

import javafind.ConsoleColor;
import javafind.FileResult;
import javafind.FileType;
import org.junit.jupiter.api.Test;

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SearchResultTest {

    public SearchResultTest() {

    }

    @Test
    public final void testSingleLineSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setColorize(false);
        final Pattern pattern = Pattern.compile("Search");
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.CODE);
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 21;
        final String line = "\tpublic class Searcher\n";
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex,
                matchEndIndex, line, Collections.emptyList(), Collections.emptyList());
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, line.trim());
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testSingleLineLongerThanMaxLineLengthSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(false);
        final Pattern pattern = Pattern.compile("maxlen");
        final String path = "./maxlen.txt";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.TEXT);
        final int lineNum = 1;
        final int matchStartIndex = 53;
        final int matchEndIndex = 59;
        final String line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, line, Collections.emptyList(), Collections.emptyList());
        final String expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testSingleLineLongerColorizeSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        final Pattern pattern = Pattern.compile("maxlen");
        final String path = "./maxlen.txt";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.TEXT);
        final int lineNum = 10;
        final int matchStartIndex = 53;
        final int matchEndIndex = 59;
        final String line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, line, Collections.emptyList(), Collections.emptyList());
        final String expectedLine = "...89012345678901234567890123456789012345678901" +
                ConsoleColor.GREEN.getValue() +
                "maxlen" +
                ConsoleColor.RESET.getValue() +
                "89012345678901234567890123456789012345678901...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testMatchLengthLongerColorizeSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        final Pattern pattern = Pattern.compile("\\d+maxlen\\d+");
        final String path = "./maxlen.txt";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.TEXT);
        final int lineNum = 10;
        final int matchStartIndex = 1;
        final int matchEndIndex = 110;
        final String line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, line, Collections.emptyList(), Collections.emptyList());
        final String expectedLine =
                ConsoleColor.GREEN.getValue() +
                "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
                ConsoleColor.RESET.getValue() +
                "...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testMatchLengthLongerColorizeSearchResult2() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        final Pattern pattern = Pattern.compile("\\d+maxlen\\d+");
        final String path = "./maxlen.txt";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.TEXT);
        final int lineNum = 10;
        final int matchStartIndex = 11;
        final int matchEndIndex = 120;
        final String line = "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ";
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, line, Collections.emptyList(), Collections.emptyList());
        final String expectedLine =
                "..." +
                ConsoleColor.GREEN.getValue() +
                "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123" +
                ConsoleColor.RESET.getValue() +
                "...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", path,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testBinaryFileSearchResult() {
        SearchSettings settings = new SearchSettings();
        final Pattern pattern = Pattern.compile("Search");
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.exe";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.BINARY);
        final int lineNum = 0;
        final int matchStartIndex = 0;
        final int matchEndIndex = 0;
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, null);
        final String expectedOutput = String.format("%s matches at [0:0]", path);
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testMultiLineSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setColorize(false);
        final Pattern pattern = Pattern.compile("Search");
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        final FileResult fileResult = new FileResult(Paths.get(path), FileType.CODE);
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 23;
        final String line = "\tpublic class Searcher\n";
        final List<String> linesBefore = Arrays.asList("namespace CsSearch\n", "{\n");
        final List<String> linesAfter = Arrays.asList("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n");
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
        final String expectedOutput = String.format(
                "================================================================================\n" +
                "%s: %d: [%d:%d]\n" +
                "--------------------------------------------------------------------------------\n" +
                "   8 | namespace CsSearch\n" +
                "   9 | {\n" +
                "> 10 | \tpublic class Searcher\n" +
                "  11 | \t{\n" +
                "  12 | \t\tprivate readonly FileTypes _fileTypes;\n",
                path, lineNum, matchStartIndex, matchEndIndex);
        final String output = new SearchResultFormatter(settings).format(searchResult);
        assertEquals(expectedOutput, output);
    }
}
