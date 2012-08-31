package scalasearch

import java.util.regex.Pattern
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite with BeforeAndAfterAll{

  test("print exts") {
    println("\nNOSEARCH_EXTS:")
    FileUtil.NOSEARCH_EXTS.toList.sortWith(_ < _).foreach(println(_))

    println("\nCOMPRESSED_EXTS:")
    FileUtil.COMPRESSED_EXTS.toList.sortWith(_ < _).foreach(println(_))

    println("\nBINARY_EXTS:")
    FileUtil.BINARY_EXTS.toList.sortWith(_ < _).foreach(println(_))

    println("\nTEXT_EXTS:")
    FileUtil.TEXT_EXTS.toList.sortWith(_ < _).foreach(println(_))

    println("\nUNKNOWN_EXTS:")
    FileUtil.UNKNOWN_EXTS.toList.sortWith(_ < _).foreach(println(_))
  }
}
