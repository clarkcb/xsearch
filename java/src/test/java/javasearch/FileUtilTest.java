package javasearch;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.*;

public class FileUtilTest {
    public FileUtilTest() {}

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
}