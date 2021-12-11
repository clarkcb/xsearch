package javasearch;

import org.junit.Test;

import java.io.File;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import static org.junit.Assert.*;

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
     * isSearchDir tests
     *************************************************************/
    @Test
    public final void testisSearchDir_SingleDot_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File(".")));
    }

    @Test
    public final void testisSearchDir_DoubleDot_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File("..")));
    }

    @Test
    public final void testisSearchDir_IsHidden_False() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertFalse(searcher.isSearchDir(new File(".git")));
    }

    @Test
    public final void testisSearchDir_IsHiddenIncludeHidden_True() {
        SearchSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File(".git")));
    }

    @Test
    public final void testisSearchDir_NoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File("/Users")));
    }

    @Test
    public final void testisSearchDir_MatchesInPattern_True() {
        SearchSettings settings = getSettings();
        settings.addInDirPattern("Search");
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File("CsSearch")));
    }

    @Test
    public final void testisSearchDir_MatchesOutPattern_False() {
        SearchSettings settings = getSettings();
        settings.addOutDirPattern("Search");
        Searcher searcher = new Searcher(settings);
        assertFalse(searcher.isSearchDir(new File("CsSearch")));
    }

    @Test
    public final void testisSearchDir_DoesNotMatchInPattern_False() {
        SearchSettings settings = getSettings();
        settings.addInDirPattern("SearchFiles");
        Searcher searcher = new Searcher(settings);
        assertFalse(searcher.isSearchDir(new File("CsSearch")));
    }

    @Test
    public final void testisSearchDir_DoesNotMatchOutPattern_True() {
        SearchSettings settings = getSettings();
        settings.addOutDirPattern("SearchFiles");
        Searcher searcher = new Searcher(settings);
        File dir = new File("CsSearch");
        assertTrue(searcher.isSearchDir(dir));
    }


    /*************************************************************
     * isSearchFile tests
     *************************************************************/

    @Test
    public final void testIsSearchFile_NoExtensionsNoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_MatchesInExtension_True() {
        SearchSettings settings = getSettings();
        settings.addInExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_DoesNotMatchInExtension_False() {
        SearchSettings settings = getSettings();
        settings.addInExtension("java");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.isSearchFile(file));
    }


    @Test
    public final void testIsSearchFile_MatchesOutExtension_False() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_DoesNotMatchOutExtension_True() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("java");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_MatchesInPattern_True() {
        SearchSettings settings = getSettings();
        settings.addInFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("Searcher.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_DoesNotMatchInPattern_False() {
        SearchSettings settings = getSettings();
        settings.addInFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_MatchesOutPattern_False() {
        SearchSettings settings = getSettings();
        settings.addOutFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("Searcher.cs");
        assertFalse(searcher.isSearchFile(file));
    }

    @Test
    public final void testIsSearchFile_DoesNotMatchOutPattern_True() {
        SearchSettings settings = getSettings();
        settings.addOutFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }


    /*************************************************************
     * IsArchiveSearchFile tests
     *************************************************************/

    @Test
    public final void testIsArchiveSearchFile_NoExtensionsNoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_MatchesInExtension_True() {
        SearchSettings settings = getSettings();
        settings.addInArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_DoesNotMatchInExtension_False() {
        SearchSettings settings = getSettings();
        settings.addInArchiveExtension("gz");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }


    @Test
    public final void testIsArchiveSearchFile_MatchesOutExtension_False() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_DoesNotMatchOutExtension_True() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveExtension("gz");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_MatchesInPattern_True() {
        SearchSettings settings = getSettings();
        settings.addInArchiveFilePattern("arch");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_DoesNotMatchInPattern_False() {
        SearchSettings settings = getSettings();
        settings.addInArchiveFilePattern("archives");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_MatchesOutPattern_False() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveFilePattern("arch");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }

    @Test
    public final void testIsArchiveSearchFile_DoesNotMatchOutPattern_True() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveFilePattern("archives");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    /*************************************************************
     * FilterFile tests
     *************************************************************/

    @Test
    public final void testFilterFile_IsHidden_False() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File(".gitignore");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_IsHiddenIncludeHidden_True() {
        SearchSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Searcher searcher = new Searcher(settings);
        File file = new File(".gitignore");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_ArchiveNoSearchArchives_False() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_ArchiveSearchArchives_True() {
        SearchSettings settings = getSettings();
        settings.setSearchArchives(true);
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_IsArchiveSearchFile_True() {
        SearchSettings settings = getSettings();
        settings.setSearchArchives(true);
        settings.addInArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_NotIsArchiveSearchFile_False() {
        SearchSettings settings = getSettings();
        settings.setSearchArchives(true);
        settings.addOutArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_ArchiveFileArchivesOnly_True() {
        SearchSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_NoExtensionsNoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_IsSearchFile_True() {
        SearchSettings settings = getSettings();
        settings.addInExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_NotIsSearchFile_False() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public final void testFilterFile_NonArchiveFileArchivesOnly_False() {
        SearchSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.filterFile(file));
    }

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
