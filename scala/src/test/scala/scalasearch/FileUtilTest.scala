package scalasearch

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite {
  /***************************************************************************
    * getExtension tests
    **************************************************************************/
  test("test get txt extension") {
    assert(FileUtil.getExtension(".filename.txt").equalsIgnoreCase("txt"))
  }

  test("test get TXT extension") {
    assert(FileUtil.getExtension(".filename.TXT").equalsIgnoreCase("txt"))
  }

  test("test get missing extension") {
    assert(FileUtil.getExtension("filename.") == "")
  }

  test("test get no extension") {
    assert(FileUtil.getExtension("filename") == "")
  }

  test("test hidden get txt extension") {
    assert(FileUtil.getExtension(".filename.txt").equalsIgnoreCase("txt"))
  }

  test("test hidden get missing extension") {
    assert(FileUtil.getExtension(".filename.") == "")
  }

  test("test hidden get no extension") {
    assert(FileUtil.getExtension(".filename") == "")
  }

  /***************************************************************************
    * isDotDir tests
    **************************************************************************/
  test("test isDotDir single dot") {
    assert(FileUtil.isDotDir("."))
  }

  test("test isDotDir double dot") {
    assert(FileUtil.isDotDir(".."))
  }

  test("test isDotDir hidden file") {
    assert(!FileUtil.isDotDir(".gitignore"))
  }

  /***************************************************************************
    * isHidden tests
    **************************************************************************/
  test("test isHidden single dot") {
    assert(!FileUtil.isHidden("."))
  }

  test("test isHidden double dot") {
    assert(!FileUtil.isHidden(".."))
  }

  test("test isHidden hidden file name") {
    assert(FileUtil.isHidden(".gitignore"))
  }

  test("test isHidden non-hidden file name") {
    assert(!FileUtil.isHidden("filename.txt"))
  }

  test("test isHidden non-hidden file path with dot") {
    assert(!FileUtil.isHidden("./filename.txt"))
  }

  /***************************************************************************
    * splitPath tests
    **************************************************************************/
  test("test splitPath path with dot") {
    val path = "./path/to/somewhere/"
    val elems = FileUtil.splitPath(path)
    assert(elems.size == 3)
    assert(elems.head == "path")
  }

  test("test splitPath path with double dot") {
    val path = "../path/to/somewhere/"
    val elems = FileUtil.splitPath(path)
    assert(elems.size == 3)
    assert(elems.head == "path")
  }

  test("test splitPath path without dot") {
    val path = "/path/to/somewhere/"
    val elems = FileUtil.splitPath(path)
    println("elems:" + elems)
    assert(elems.size == 3)
    assert(elems.head == "path")
  }
}
