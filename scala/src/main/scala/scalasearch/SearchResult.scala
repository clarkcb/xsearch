package scalasearch

import java.io.File
import scala.util.matching.Regex

class SearchResult(val searchPattern: Regex, val file: File, val lineNum: Int, val line: String) {
  override def toString() = {
    val matchString =
      if (lineNum == 0) " matches"
      else ": %d: %s".format(lineNum, line.trim)
    file.getPath + matchString
  }
}
