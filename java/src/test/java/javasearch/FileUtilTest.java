/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.junit.Test;

public class FileUtilTest {
    FileUtil fileUtil;

    public FileUtilTest() {
        fileUtil = new FileUtil();
    }

    @Test
    public void binaryFileTest() {
        File file = new File("test.exe");
        assertEquals(fileUtil.getExtension(file), "exe");
        assertFalse(fileUtil.isArchiveFile(file));
        assertTrue(fileUtil.isBinaryFile(file));
        assertTrue(fileUtil.isSearchableFile(file));
        assertFalse(fileUtil.isTextFile(file));
    }

    @Test
    public void javaFileTest() {
        File file = new File("Test.java");
        assertEquals(fileUtil.getExtension(file), "java");
        assertFalse(fileUtil.isArchiveFile(file));
        assertFalse(fileUtil.isBinaryFile(file));
        assertTrue(fileUtil.isSearchableFile(file));
        assertTrue(fileUtil.isTextFile(file));
    }

    @Test
    public void textFileTest() {
        File file = new File("test.txt");
        assertEquals(fileUtil.getExtension(file), "txt");
        assertFalse(fileUtil.isArchiveFile(file));
        assertFalse(fileUtil.isBinaryFile(file));
        assertTrue(fileUtil.isSearchableFile(file));
        assertTrue(fileUtil.isTextFile(file));
    }

    @Test
    public void compressedFileTest() {
        File file = new File("test.zip");
        assertEquals(fileUtil.getExtension(file), "zip");
        assertTrue(fileUtil.isArchiveFile(file));
        assertFalse(fileUtil.isBinaryFile(file));
        assertTrue(fileUtil.isSearchableFile(file));
        assertFalse(fileUtil.isTextFile(file));
    }
}