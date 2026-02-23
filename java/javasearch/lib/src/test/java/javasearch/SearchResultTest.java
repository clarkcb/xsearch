package javasearch;

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
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SearchResultTest {

    public SearchResultTest() {

    }

    public static SearchResult getSearchResult(String patternString, String pathString, FileType fileType,
                                               int lineNum, int matchStartIdx, int matchEndIdx, String line) {
        return getSearchResult(patternString, pathString, fileType, lineNum, matchStartIdx, matchEndIdx, line,
                Collections.emptyList(), Collections.emptyList());
    }

    public static SearchResult getSearchResult(String patternString, String pathString, FileType fileType,
                                               int lineNum, int matchStartIdx, int matchEndIdx, String line,
                                               List<String> linesBefore, List<String> linesAfter) {
        final Pattern pattern = Pattern.compile(patternString);
        final Path path = Paths.get(pathString);
        final FileResult fileResult = new FileResult(path, fileType);
        return new SearchResult(pattern, fileResult, lineNum, matchStartIdx,
                matchEndIdx, line, linesBefore, linesAfter);
    }

    @Test
    public final void testSingleLineSearchResult() {
        final String pattern = "Search";
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 21;
        final String line = "\tpublic class Searcher\n";
        final SearchResult searchResult = getSearchResult(pattern, path, FileType.CODE, lineNum, matchStartIndex,
                matchEndIndex, line);

        assertEquals(pattern, searchResult.getSearchPattern().pattern());
        assertEquals(path, searchResult.getFileResult().toString());
        assertEquals(lineNum, searchResult.getLineNum());
        assertEquals(matchStartIndex, searchResult.getMatchStartIndex());
        assertEquals(matchEndIndex, searchResult.getMatchEndIndex());
        assertEquals(line, searchResult.getLine());
        assertTrue(searchResult.getLinesBefore().isEmpty());
        assertTrue(searchResult.getLinesAfter().isEmpty());
    }

    @Test
    public final void testMultiLineSearchResult() {
        SearchSettings settings = new SearchSettings();
        settings.setColorize(false);
        final String pattern = "Search";
        final String path = "~/src/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
        final int lineNum = 10;
        final int matchStartIndex = 15;
        final int matchEndIndex = 23;
        final String line = "\tpublic class Searcher\n";
        final List<String> linesBefore = Arrays.asList("namespace CsSearch\n", "{\n");
        final List<String> linesAfter = Arrays.asList("\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n");
        final SearchResult searchResult = getSearchResult(pattern, path, FileType.CODE, lineNum, matchStartIndex,
                matchEndIndex, line, linesBefore, linesAfter);

        assertEquals(pattern, searchResult.getSearchPattern().pattern());
        assertEquals(path, searchResult.getFileResult().toString());
        assertEquals(lineNum, searchResult.getLineNum());
        assertEquals(matchStartIndex, searchResult.getMatchStartIndex());
        assertEquals(matchEndIndex, searchResult.getMatchEndIndex());
        assertEquals(line, searchResult.getLine());
        assertEquals(2, searchResult.getLinesBefore().size());
        assertEquals(2, searchResult.getLinesAfter().size());
    }
}
