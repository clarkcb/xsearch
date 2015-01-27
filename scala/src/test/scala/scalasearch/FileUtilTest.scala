package scalasearch

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite {
  test("test get txt extension") {
    assert(FileUtil.getExtension("filename.txt") == "txt")
  }

  test("test get missing extension") {
    assert(FileUtil.getExtension("filename.") == "")
  }

  test("test get no extension") {
    assert(FileUtil.getExtension("filename") == "")
  }

  test("test hidden get txt extension") {
    assert(FileUtil.getExtension(".filename.txt") == "txt")
  }

  test("test hidden get missing extension") {
    assert(FileUtil.getExtension(".filename.") == "")
  }

  test("test hidden get no extension") {
    assert(FileUtil.getExtension(".filename") == "")
  }
}
