package scalasearch

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File

class SearchFileTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test("test_searchfile_abs_path") {
    val filepath = "~/src/xsearch/scala/scalasearch/src/main/scala/scalasearch/SearchFile.scala"
    val searchFile = new SearchFile(new File(filepath), FileType.Code)
    assertEquals(filepath, searchFile.toString())
  }

  test("test_searchfile_rel_path1") {
    val filepath = "./SearchFile.scala"
    val searchFile = new SearchFile(new File(filepath), FileType.Code)
    assertEquals(filepath, searchFile.toString())
  }

  test("test_searchfile_rel_path2") {
    val filepath = "../SearchFile.scala"
    val searchFile = new SearchFile(new File(filepath), FileType.Code)
    assertEquals(filepath, searchFile.toString())
  }
}
