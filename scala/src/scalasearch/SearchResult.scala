import java.io._
import scala.io._

class SearchResult(val searchString: String, val file: File, val lineNum: Int, val line: String) {
    override def toString() = {
        file.getPath + ": " + lineNum + ": " + line.trim
    }
}
