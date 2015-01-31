package javasearch;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.*;

public class SearcherTest {

    public SearcherTest() {}

    private static SearchSettings getSettings() {
        SearchSettings settings = new SearchSettings();
        settings.setStartPath(".");
        settings.addSearchPattern("Searcher");
        return settings;
    }


    /*************************************************************
     * isSearchDir tests
     *************************************************************/
    @Test
    public void testisSearchDir_SingleDot_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File(".")));
    }

    @Test
    public void testisSearchDir_DoubleDot_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File("..")));
    }

    @Test
    public void testisSearchDir_IsHidden_False() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertFalse(searcher.isSearchDir(new File(".git")));
    }

    @Test
    public void testisSearchDir_IsHiddenIncludeHidden_True() {
        SearchSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File(".git")));
    }

    @Test
    public void testisSearchDir_NoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File("/Users")));
    }

    @Test
    public void testisSearchDir_MatchesInPattern_True() {
        SearchSettings settings = getSettings();
        settings.addInDirPattern("Search");
        Searcher searcher = new Searcher(settings);
        assertTrue(searcher.isSearchDir(new File("CsSearch")));
    }

    @Test
    public void testisSearchDir_MatchesOutPattern_False() {
        SearchSettings settings = getSettings();
        settings.addOutDirPattern("Search");
        Searcher searcher = new Searcher(settings);
        assertFalse(searcher.isSearchDir(new File("CsSearch")));
    }

    @Test
    public void testisSearchDir_DoesNotMatchInPattern_False() {
        SearchSettings settings = getSettings();
        settings.addInDirPattern("SearchFiles");
        Searcher searcher = new Searcher(settings);
        assertFalse(searcher.isSearchDir(new File("CsSearch")));
    }

    @Test
    public void testisSearchDir_DoesNotMatchOutPattern_True() {
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
    public void testIsSearchFile_NoExtensionsNoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_MatchesInExtension_True() {
        SearchSettings settings = getSettings();
        settings.addInExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_DoesNotMatchInExtension_False() {
        SearchSettings settings = getSettings();
        settings.addInExtension("java");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.isSearchFile(file));
    }


    @Test
    public void testIsSearchFile_MatchesOutExtension_False() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_DoesNotMatchOutExtension_True() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("java");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_MatchesInPattern_True() {
        SearchSettings settings = getSettings();
        settings.addInFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("Searcher.cs");
        assertTrue(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_DoesNotMatchInPattern_False() {
        SearchSettings settings = getSettings();
        settings.addInFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_MatchesOutPattern_False() {
        SearchSettings settings = getSettings();
        settings.addOutFilePattern("Search");
        Searcher searcher = new Searcher(settings);
        File file = new File("Searcher.cs");
        assertFalse(searcher.isSearchFile(file));
    }

    @Test
    public void testIsSearchFile_DoesNotMatchOutPattern_True() {
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
    public void testIsArchiveSearchFile_NoExtensionsNoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_MatchesInExtension_True() {
        SearchSettings settings = getSettings();
        settings.addInArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_DoesNotMatchInExtension_False() {
        SearchSettings settings = getSettings();
        settings.addInArchiveExtension("gz");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }


    @Test
    public void testIsArchiveSearchFile_MatchesOutExtension_False() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_DoesNotMatchOutExtension_True() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveExtension("gz");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_MatchesInPattern_True() {
        SearchSettings settings = getSettings();
        settings.addInArchiveFilePattern("arch");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_DoesNotMatchInPattern_False() {
        SearchSettings settings = getSettings();
        settings.addInArchiveFilePattern("archives");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_MatchesOutPattern_False() {
        SearchSettings settings = getSettings();
        settings.addOutArchiveFilePattern("arch");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.isArchiveSearchFile(file));
    }

    @Test
    public void testIsArchiveSearchFile_DoesNotMatchOutPattern_True() {
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
    public void testFilterFile_IsHidden_False() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File(".gitignore");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_IsHiddenIncludeHidden_True() {
        SearchSettings settings = getSettings();
        settings.setExcludeHidden(false);
        Searcher searcher = new Searcher(settings);
        File file = new File(".gitignore");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_ArchiveNoSearchArchives_False() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_ArchiveSearchArchives_True() {
        SearchSettings settings = getSettings();
        settings.setSearchArchives(true);
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_IsArchiveSearchFile_True() {
        SearchSettings settings = getSettings();
        settings.setSearchArchives(true);
        settings.addInArchiveExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_NotIsArchiveSearchFile_False() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("zip");
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_ArchiveFileArchivesOnly_True() {
        SearchSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Searcher searcher = new Searcher(settings);
        File file = new File("archive.zip");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_NoExtensionsNoPatterns_True() {
        SearchSettings settings = getSettings();
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_IsSearchFile_True() {
        SearchSettings settings = getSettings();
        settings.addInExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertTrue(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_NotIsSearchFile_False() {
        SearchSettings settings = getSettings();
        settings.addOutExtension("cs");
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.filterFile(file));
    }

    @Test
    public void testFilterFile_NonArchiveFileArchivesOnly_False() {
        SearchSettings settings = getSettings();
        settings.setArchivesOnly(true);
        Searcher searcher = new Searcher(settings);
        File file = new File("FileUtil.cs");
        assertFalse(searcher.filterFile(file));
    }
}