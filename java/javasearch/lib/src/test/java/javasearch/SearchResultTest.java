package javasearch;

import javafind.FileResult;
import javafind.FileType;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SearchResultTest {

    public SearchResultTest() {

    }

    @Test
    public final void testSingleLineSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(DefaultSearchSettings.MAX_LINE_LENGTH);
        settings.setColorize(false);
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        final Pattern pattern = Pattern.compile("Search");
        final Path path = Paths.get("~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs");
        final FileResult fileResult = new FileResult(path, FileType.CODE);
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 21;
        final String line = "\tpublic class Searcher\n";
        final List<String> beforeAfterLines = new ArrayList<>();
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum, matchStartIndex,
                matchEndIndex, line, beforeAfterLines, beforeAfterLines);
        final String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch" + File.separator + "Searcher.cs";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, line.trim());
        final String output = formatter.format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testSingleLineLongerThanMaxLineLengthSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(false);
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        final Pattern pattern = Pattern.compile("maxlen");
        final Path path = Paths.get("./maxlen.txt");
        final FileResult fileResult = new FileResult(path, FileType.TEXT);
        final int lineNum = 1;
        final int matchStartIndex = 53;
        final int matchEndIndex = 59;
        final String line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final List<String> linesBeforeAfter = new ArrayList<>();
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, line, linesBeforeAfter, linesBeforeAfter);
        final String expectedPath = "." + File.separator + "maxlen.txt";
        final String expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);
        final String output = formatter.format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testSingleLineLongerColorizeSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setMaxLineLength(100);
        settings.setColorize(true);
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        final Pattern pattern = Pattern.compile("maxlen");
        final Path path = Paths.get("./maxlen.txt");
        final FileResult fileResult = new FileResult(path, FileType.TEXT);
        final int lineNum = 10;
        final int matchStartIndex = 53;
        final int matchEndIndex = 59;
        final String line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        final List<String> linesBeforeAfter = new ArrayList<>();
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, line, linesBeforeAfter, linesBeforeAfter);
        final String expectedPath = "." + File.separator + "maxlen.txt";
        final String expectedLine = "...89012345678901234567890123456789012345678901" +
                Color.GREEN +
                "maxlen" +
                Color.RESET +
                "89012345678901234567890123456789012345678901...";
        final String expectedOutput = String.format("%s: %d: [%d:%d]: %s", expectedPath,
                lineNum, matchStartIndex, matchEndIndex, expectedLine);
        final String output = formatter.format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testBinaryFileSearchResult() {
        SearchSettings settings = new SearchSettings();
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        final Pattern pattern = Pattern.compile("Search");
        final Path path = Paths.get("~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe");
        final FileResult fileResult = new FileResult(path, FileType.BINARY);
        final int lineNum = 0;
        final int matchStartIndex = 0;
        final int matchEndIndex = 0;
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
            matchStartIndex, matchEndIndex, null);
        final String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch" + File.separator + "Searcher.exe";
        final String expectedOutput = String.format("%s matches at [0:0]", expectedPath);
        final String output = formatter.format(searchResult);
        assertEquals(expectedOutput, output);
    }

    @Test
    public final void testMultiLineSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setColorize(false);
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        final Pattern pattern = Pattern.compile("Search");
        final Path path = Paths.get("~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs");
        final FileResult fileResult = new FileResult(path, FileType.CODE);
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 23;
        final String line = "\tpublic class Searcher\n";
        final List<String> linesBefore = Arrays.asList("namespace CsSearch\n", "{\n");
        final List<String> linesAfter = Arrays.asList("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n");
        final SearchResult searchResult = new SearchResult(pattern, fileResult, lineNum,
                matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);
        final String expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch" + File.separator + "Searcher.cs";
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
        final String output = formatter.format(searchResult);
        assertEquals(expectedOutput, output);
    }
}
