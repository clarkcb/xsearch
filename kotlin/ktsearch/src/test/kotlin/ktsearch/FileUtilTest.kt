package ktsearch

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.File

/**
 * @author cary on 7/30/16.
 */
class FileUtilTest {

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    @Test
    fun testIsDotDirSingleDot() {
        val filename = "."
        assertTrue(FileUtil.isDotDir(filename))
    }

    @Test
    fun testIsDotDirDoubleDot() {
        val filename = ".."
        assertTrue(FileUtil.isDotDir(filename))
    }

    @Test
    fun testIsDotDirNotDotDir() {
        val filename = "~/path"
        assertFalse(FileUtil.isDotDir(filename))
    }

    @Test
    fun testIsDotDirPathWithDot() {
        val filename = "./path"
        assertFalse(FileUtil.isDotDir(filename))
    }

    @Test
    fun testIsDotDirHiddenFile() {
        val filename = ".gitignore"
        assertFalse(FileUtil.isDotDir(filename))
    }

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    @Test
    fun testIsHiddenSingleDot() {
        val filename = "."
        assertFalse(FileUtil.isHidden(filename))
    }

    @Test
    fun testIsHiddenDoubleDot() {
        val filename = ".."
        assertFalse(FileUtil.isHidden(filename))
    }

    @Test
    fun testIsHiddenHiddenFileName() {
        val filename = ".gitignore"
        assertTrue(FileUtil.isHidden(filename))
    }

    @Test
    fun testIsHiddenNotHiddenFileName() {
        val filename = "file.txt"
        assertFalse(FileUtil.isHidden(filename))
    }
}
