package javasearch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.junit.Test;

public class FileTypesTest {
    private final FileTypes fileTypes;

    public FileTypesTest() {
        fileTypes = new FileTypes();
    }

    @Test
    public final void archiveFileTest() {
        File file = new File("test.zip");
        assertEquals("zip", FileUtil.getExtension(file));
        assertTrue(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertFalse(fileTypes.isCodeFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertFalse(fileTypes.isTextFile(file));
        assertFalse(fileTypes.isXmlFile(file));
        assertEquals(FileType.ARCHIVE, fileTypes.getFileType(file));
    }

    @Test
    public final void binaryFileTest() {
        File file = new File("test.exe");
        assertEquals("exe", FileUtil.getExtension(file));
        assertFalse(fileTypes.isArchiveFile(file));
        assertTrue(fileTypes.isBinaryFile(file));
        assertFalse(fileTypes.isCodeFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertFalse(fileTypes.isTextFile(file));
        assertFalse(fileTypes.isXmlFile(file));
        assertEquals(FileType.BINARY, fileTypes.getFileType(file));
    }

    @Test
    public final void codeFileTest() {
        File file = new File("Test.java");
        assertEquals("java", FileUtil.getExtension(file));
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertTrue(fileTypes.isCodeFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertTrue(fileTypes.isTextFile(file));
        assertFalse(fileTypes.isXmlFile(file));
        assertEquals(FileType.CODE, fileTypes.getFileType(file));
    }

    @Test
    public final void textFileTest() {
        File file = new File("test.txt");
        assertEquals("txt", FileUtil.getExtension(file));
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertFalse(fileTypes.isCodeFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertTrue(fileTypes.isTextFile(file));
        assertFalse(fileTypes.isXmlFile(file));
        assertEquals(FileType.TEXT, fileTypes.getFileType(file));
    }

    @Test
    public final void xmlFileTest() {
        File file = new File("markup.xml");
        assertEquals("xml", FileUtil.getExtension(file));
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertFalse(fileTypes.isCodeFile(file));
        assertTrue(fileTypes.isSearchableFile(file));
        assertTrue(fileTypes.isTextFile(file));
        assertTrue(fileTypes.isXmlFile(file));
        assertEquals(FileType.XML, fileTypes.getFileType(file));
    }

    @Test
    public final void unknownFileTest() {
        File file = new File("unknown.ZZZ");
        assertEquals("zzz", FileUtil.getExtension(file));
        assertFalse(fileTypes.isArchiveFile(file));
        assertFalse(fileTypes.isBinaryFile(file));
        assertFalse(fileTypes.isSearchableFile(file));
        assertFalse(fileTypes.isTextFile(file));
        assertFalse(fileTypes.isXmlFile(file));
        assertTrue(fileTypes.isUnknownFile(file));
        assertEquals(FileType.UNKNOWN, fileTypes.getFileType(file));
    }
}
