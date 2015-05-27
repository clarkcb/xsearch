package javasearch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.junit.Test;

public class FileTypesTest {
    private FileTypes fileTypes;

    public FileTypesTest() {
        fileTypes = new FileTypes();
    }

    @Test
    public final void archiveFileTest() {
        File file = new File("test.zip");
        assertEquals(FileUtil.getExtension(file), "zip");
        assertTrue(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertFalse(fileTypes.isTextFile(file));
        assertEquals(fileTypes.getFileType(file), FileType.ARCHIVE);
    }

    @Test
    public final void binaryFileTest() {
        File file = new File("test.exe");
        assertEquals(FileUtil.getExtension(file), "exe");
        assertFalse(fileTypes.isArchiveFile(file));
        assertTrue(fileTypes.isBinaryFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertFalse(fileTypes.isTextFile(file));
        assertEquals(fileTypes.getFileType(file), FileType.BINARY);
    }

    @Test
    public final void javaFileTest() {
        File file = new File("Test.java");
        assertEquals(FileUtil.getExtension(file), "java");
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertTrue(fileTypes.isTextFile(file));
        assertEquals(fileTypes.getFileType(file), FileType.TEXT);
    }

    @Test
    public final void textFileTest() {
        File file = new File("test.txt");
        assertEquals(FileUtil.getExtension(file), "txt");
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertTrue(fileTypes.isTextFile(file));
        assertEquals(fileTypes.getFileType(file), FileType.TEXT);
    }

    @Test
    public final void unknownFileTest() {
        File file = new File("unknown.ZZZ");
        assertEquals(FileUtil.getExtension(file), "zzz");
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertFalse(fileTypes.isSearchableFile(file));
        assertFalse(fileTypes.isTextFile(file));
        assertTrue(fileTypes.isUnknownFile(file));
        assertEquals(fileTypes.getFileType(file), FileType.UNKNOWN);
    }
}
