package ktsearch

import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

class SearchFileTest {

    @Test
    fun test_searchfile_abs_path() {
        val path = "/Users/cary/src/xsearch/kotlin/ktsearch/src/main/kotlin/ktsearch/SearchFile.kt"
        val searchFile = SearchFile(File(path), FileType.CODE)
        assertEquals(path, searchFile.toString())
    }

    @Test
    fun test_searchfile_tilde_path() {
        val path = "~/src/xsearch/kotlin/ktsearch/src/main/kotlin/ktsearch/SearchFile.kt"
        val searchFile = SearchFile(File(path), FileType.CODE)
        assertEquals(path, searchFile.toString())
    }

    @Test
    fun test_searchfile_rel_path1() {
        val path = "./SearchFile.kt"
        val searchFile = SearchFile(File(path), FileType.CODE)
        assertEquals(path, searchFile.toString())
    }

    @Test
    fun test_searchfile_rel_path2() {
        val path = "../SearchFile.kt"
        val searchFile = SearchFile(File(path), FileType.CODE)
        assertEquals(path, searchFile.toString())
    }
}
