package scalasearch

import java.io.File

class SearchFile(val containers: List[String], val file: File, val fileType: FileType.Value) {

  val CONTAINER_SEPARATOR = "!"

  def this(file: File, fileType: FileType.Value) = {
    this(List.empty[String], file, fileType)
  }

  // get just the path inside the container(s)
  def getPath : String = {
    file.getPath
  }

  // get just the path inside the container(s)
  def getPathWithContainers : String = {
    toString
  }

  override def toString : String = {
    val sb = new StringBuilder
    if (containers.nonEmpty) {
      sb.append(containers.mkString(CONTAINER_SEPARATOR)).append(CONTAINER_SEPARATOR)
    }
    sb.append(file.getPath)
    sb.toString()
  }
}
