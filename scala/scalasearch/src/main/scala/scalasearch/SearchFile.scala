package scalasearch

import java.io.File

class SearchFile(val containers: List[String], val path: String,
                 val fileName: String, val fileType: FileType.Value) {

  val CONTAINER_SEPARATOR = "!"

  def this(path: String, file: String, fileType: FileType.Value) = {
    this(List.empty[String], path, file, fileType)
  }

  def toFile: File = {
    val p = new File(path)
    new File(p, fileName)
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
    sb.append(fileName)
    sb.toString()
  }
}
