package scalasearch

import java.io.File
import java.util.regex.Pattern
//import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.junit.JUnitRunner

//@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite with BeforeAndAfterAll{

  val binFile = new File("binFile.exe")
  val textFile = new File("textFile.txt")

  test("test bin file") {
    assert(FileUtil.isBinaryFile(binFile))
    assert(!FileUtil.isCompressedFile(binFile))
    assert(FileUtil.isSearchableFile(binFile))
    assert(!FileUtil.isTextFile(binFile))
  }

  test("test text file") {
    assert(!FileUtil.isBinaryFile(textFile))
    assert(!FileUtil.isCompressedFile(textFile))
    assert(FileUtil.isSearchableFile(textFile))
    assert(FileUtil.isTextFile(textFile))
  }
}
