package scalasearch

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class SearchFileTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test("test_searchfile_abs_path") {
    val path = "~/src/xsearch/scala/scalasearch/src/main/scala/scalasearch"
    val filename = "SearchFile.scala"
    val filepath = path + "/" + filename
    val searchFile = new SearchFile(path, filename, FileType.Code)
    assertEquals(filepath, searchFile.toString())
  }

  test("test_searchfile_rel_path1") {
    val path = "."
    val filename = "SearchFile.scala"
    val filepath = path + "/" + filename
    val searchFile = new SearchFile(path, filename, FileType.Code)
    assertEquals(filepath, searchFile.toString())
  }

  test("test_searchfile_rel_path2") {
    val path = ".."
    val filename = "SearchFile.scala"
    val filepath = path + "/" + filename
    val searchFile = new SearchFile(path, filename, FileType.Code)
    assertEquals(filepath, searchFile.toString())
  }
}
