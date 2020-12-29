package scalasearch

import java.io.File
import scala.io.{Codec, Source}

object FileUtil {
  val CURRENT_PATH = "."
  val PARENT_PATH = ".."

  def getExtension(f: SearchFile): String = {
    getExtension(f.file.getName)
  }

  def getExtension(name: String): String = {
    val lastIndex = name.lastIndexOf('.')
    if (lastIndex > 0 && lastIndex < name.length-1) {
      name.split('.').last
    } else {
      ""
    }
  }

  def getFileContents(f: File, enc: String=DefaultSettings.textFileEncoding): String = {
    val bufferedSource = Source.fromFile(f, enc)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }

  def getFileContents(f: File, codec: Codec): String = {
    val bufferedSource = Source.fromFile(f)(codec)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }

  def isDotDir(name: String): Boolean = {
    Set(CURRENT_PATH, PARENT_PATH).contains(name)
  }

  def isHidden(name: String): Boolean = {
    val n = new File(name).getName
    n.length > 1 && n.startsWith(CURRENT_PATH) && !isDotDir(n)
  }

  def pathOrCurrent(f: File): File = {
    Option(f.getParentFile) match {
      case Some(dir) => dir
      case None => new File(CURRENT_PATH)
    }
  }
  def splitPath(path: String): Iterable[String] = {
    path.split(File.separator).filterNot(_.isEmpty).filterNot(isDotDir)
  }
}
