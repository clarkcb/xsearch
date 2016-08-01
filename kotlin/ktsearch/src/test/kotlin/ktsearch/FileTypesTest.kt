package ktsearch

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import java.io.File

/**
 * @author cary on 7/30/16.
 */
class FileTypesTest {
    private val fileTypes = FileTypes()

    @Test
    fun archiveFileTest() {
        val file = File("test.zip")
        assertEquals(file.extension, "zip")
        assertTrue(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isSearchableFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertEquals(fileTypes.getFileType(file), FileType.ARCHIVE)
    }

    @Test
    fun binaryFileTest() {
        val file = File("test.exe")
        assertEquals(file.extension, "exe")
        assertFalse(fileTypes.isArchiveFile(file))
        assertTrue(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isSearchableFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertEquals(fileTypes.getFileType(file), FileType.BINARY)
    }

    @Test
    fun javaFileTest() {
        val file = File("Test.java")
        assertEquals(file.extension, "java")
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isSearchableFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertEquals(fileTypes.getFileType(file), FileType.TEXT)
    }

    @Test
    fun textFileTest() {
        val file = File("test.txt")
        assertEquals(file.extension, "txt")
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isSearchableFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertEquals(fileTypes.getFileType(file), FileType.TEXT)
    }

    @Test
    fun textFileTestUppercase() {
        val file = File("TEXT.TXT")
        assertEquals(file.extension, "TXT")
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isSearchableFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertEquals(fileTypes.getFileType(file), FileType.TEXT)
    }

    @Test
    fun unknownFileTest() {
        val file = File("unknown.ZZZ")
        assertEquals(file.extension, "ZZZ")
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isSearchableFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertTrue(fileTypes.isUnknownFile(file))
        assertEquals(fileTypes.getFileType(file), FileType.UNKNOWN)
    }
}
