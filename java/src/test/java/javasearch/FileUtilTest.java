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
    public void testGetTxtExtension() {
        File file = new File("filename.txt");
        assertEquals(FileUtil.getExtension(file), "txt");
    }

    @Test
    public void testGetMissingExtension() {
        File file = new File("filename.");
        assertEquals(FileUtil.getExtension(file), "");
    }

    @Test
    public void testGetNoExtension() {
        File file = new File("filename");
        assertEquals(FileUtil.getExtension(file), "");
    }

    @Test
    public void testGetHiddenTxtExtension() {
        File file = new File(".filename.txt");
        assertEquals(FileUtil.getExtension(file), "txt");
    }

    @Test
    public void testGetHiddenMissingExtension() {
        File file = new File("filename.");
        assertEquals(FileUtil.getExtension(file), "");
    }

    @Test
    public void testGetHiddenNoExtension() {
        File file = new File("filename");
        assertEquals(FileUtil.getExtension(file), "");
    }

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    @Test
    public void testIsDotDirSingleDot() {
        String filename = ".";
        assert(FileUtil.isDotDir(filename));
    }

    @Test
    public void testIsDotDirDoubleDot() {
        String filename = "..";
        assert(FileUtil.isDotDir(filename));
    }

    @Test
    public void testIsDotDirNotDotDir() {
        String filename = "~/path";
        assert(!FileUtil.isDotDir(filename));
    }

    @Test
    public void testIsDotDirPathWithDot() {
        String filename = "./path";
        assert(!FileUtil.isDotDir(filename));
    }

    @Test
    public void testIsDotDirHiddenFile() {
        String filename = ".gitignore";
        assert(!FileUtil.isDotDir(filename));
    }

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    @Test
    public void testIsHiddenSingleDot() {
        String filename = ".";
        assert(!FileUtil.isHidden(filename));
    }

    @Test
    public void testIsHiddenDoubleDot() {
        String filename = "..";
        assert(!FileUtil.isHidden(filename));
    }

    @Test
    public void testIsHiddenHiddenFileName() {
        String filename = ".gitignore";
        assert(FileUtil.isHidden(filename));
    }

    @Test
    public void testIsHiddenNotHiddenFileName() {
        String filename = "file.txt";
        assert(!FileUtil.isHidden(filename));
    }

    @Test
    public void testIsHiddenHiddenFile() {
        File file = new File(".gitignore");
        assert(FileUtil.isHidden(file));
    }

    @Test
    public void testIsHiddenNotHiddenFile() {
        File file = new File("./file.txt");
        assert(!FileUtil.isHidden(file));
    }

    /***************************************************************************
     * splitPath tests
     **************************************************************************/
    @Test
    public void testSplitPathWithDot() {
        String path = "./path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assert(elems.size() == 3);
        assert(elems.get(0).equals("path"));
    }

    @Test
    public void testSplitPathWithDoubleDot() {
        String path = "../path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assert(elems.size() == 3);
        assert(elems.get(0).equals("path"));
    }

    @Test
    public void testSplitPathWithoutDot() {
        String path = "/path/to/somewhere/";
        List<String> elems = FileUtil.splitPath(path);
        assert(elems.size() == 3);
        assert(elems.get(0).equals("path"));
    }
}











