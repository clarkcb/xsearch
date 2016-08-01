package ktsearch

import org.junit.Test
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

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
}
