package scalasearch

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite {
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

  test("test isDotDir hidden file") {
    assert(!FileUtil.isDotDir(".gitignore"))
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

  test("test isDotDir single dot") {
    assert(FileUtil.isDotDir("."))
  }

  test("test isDotDir double dot") {
    assert(FileUtil.isDotDir(".."))
  }

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
}
