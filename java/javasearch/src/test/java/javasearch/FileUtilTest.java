package javasearch;

import org.junit.Test;

import java.io.File;
import java.util.List;

import static org.junit.Assert.*;

public class FileUtilTest {
    public FileUtilTest() {}

    /***************************************************************************
     * getExtension tests
     **************************************************************************/
    @Test
    public final void testGetTxtExtension() {
        File file = new File("filename.txt");
        assertEquals(FileUtil.getExtension(file), "txt");
    }

    @Test
    public final void testGetMissingExtension() {
        File file = new File("filename.");
        assertEquals(FileUtil.getExtension(file), "");
    }

    @Test
    public final void testGetNoExtension() {
        File file = new File("filename");
        assertEquals(FileUtil.getExtension(file), "");
    }

    @Test
    public final void testGetHiddenTxtExtension() {
        File file = new File(".filename.txt");
        assertEquals(FileUtil.getExtension(file), "txt");
    }

    @Test
    public final void testGetHiddenMissingExtension() {
        File file = new File("filename.");
        assertEquals(FileUtil.getExtension(file), "");
    }

    @Test
    public final void testGetHiddenNoExtension() {
        File file = new File("filename");
        assertEquals(FileUtil.getExtension(file), "");
    }

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    @Test
    public final void testIsDotDirSingleDot() {
        String filename = ".";
        assertTrue(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirDoubleDot() {
        String filename = "..";
        assertTrue(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirNotDotDir() {
        String filename = "~/path";
        assertFalse(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirPathWithDot() {
        String filename = "./path";
        assertFalse(FileUtil.isDotDir(filename));
    }

    @Test
    public final void testIsDotDirHiddenFile() {
        String filename = ".gitignore";
        assertFalse(FileUtil.isDotDir(filename));
    }

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    @Test
    public final void testIsHiddenSingleDot() {
        String filename = ".";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenDoubleDot() {
        String filename = "..";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenHiddenFileName() {
        String filename = ".gitignore";
        assertTrue(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenNotHiddenFileName() {
        String filename = "file.txt";
        assertFalse(FileUtil.isHidden(filename));
    }

    @Test
    public final void testIsHiddenHiddenFile() {
        File file = new File(".gitignore");
        assertTrue(FileUtil.isHidden(file));
    }

    @Test
    public final void testIsHiddenNotHiddenFile() {
        File file = new File("./file.txt");
        assertFalse(FileUtil.isHidden(file));
    }

    /***************************************************************************
     * splitPath tests
     **************************************************************************/
    @Test
    public final void testSplitPathWithDot() {
        String path = "./path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 3);
        assertTrue(elems.get(0).equals("path"));
    }

    @Test
    public final void testSplitPathWithDoubleDot() {
        String path = "../path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 3);
        assertTrue(elems.get(0).equals("path"));
    }

    @Test
    public final void testSplitPathWithoutDot() {
        String path = "/path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assertEquals(elems.size(), 3);
        assertTrue(elems.get(0).equals("path"));
    }
}
