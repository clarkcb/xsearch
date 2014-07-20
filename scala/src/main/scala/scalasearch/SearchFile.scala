package scalasearch

import java.io.File

class SearchFile(val containers: List[String], val path: String, val file: String) {

  val CONTAINER_SEPARATOR = "!"

  def this(path: String, file: String) = {
    this(List.empty[String], path, file)
  }

  def toFile: File = {
    val p = new File(path)
    new File(p, file)
  }

  // get just the path inside the container(s)
  def getPath = {
    toFile.getPath
  }

  // get just the path inside the container(s)
  def getPathWithContainers = {
    toString
  }

  override def toString = {
    val sb = new StringBuilder
    if (containers.nonEmpty) {
      sb.append(containers.mkString(CONTAINER_SEPARATOR)).append(CONTAINER_SEPARATOR)
    }
    if (path.nonEmpty) {
      sb.append(path).append(File.separator)
    }
    sb.append(file)
    sb.toString()
  }
}

object SearchFileType extends Enumeration {
  type SearchFileType = Value
  val NonSearchFile = Value // a file that does not match search criteria
  val SearchFile = Value // a regular file that matches search criteria
  val ArchiveSearchFile = Value // an archive file that matches search criteria
}
