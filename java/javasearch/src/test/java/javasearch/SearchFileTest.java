package javasearch;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class SearchFileTest {

    public SearchFileTest() {}

    @Test
    public final void test_searchfile_abs_path() {
        String path = "~/src/xsearch/java/javasearch/src/main/java/javasearch";
        String filename = "SearchFile.java";
        SearchFile searchFile = new SearchFile(path, filename, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, searchFile.toString());
    }

    @Test
    public final void test_searchfile_rel_path1() {
        String path = ".";
        String filename = "SearchFile.java";
        SearchFile searchFile = new SearchFile(path, filename, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, searchFile.toString());
    }

    @Test
    public final void test_searchfile_rel_path2() {
        String path = "..";
        String filename = "SearchFile.java";
        SearchFile searchFile = new SearchFile(path, filename, FileType.CODE);
        String expectedFilePath = path + "/" + filename;
        assertEquals(expectedFilePath, searchFile.toString());
    }
}
