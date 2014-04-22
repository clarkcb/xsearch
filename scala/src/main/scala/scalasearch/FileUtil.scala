package scalasearch

import java.io.File
import scala.collection.mutable
import scala.xml._

object FileUtil {
  private val _fileTypesXmlPath = "/filetypes.xml"
  private val _fileTypeMap = mutable.Map.empty[String, Set[String]]

  private def fileTypeMap: Map[String, Set[String]] = {
    if (_fileTypeMap.isEmpty) {
      val fileTypeMap = mutable.Map.empty[String, Set[String]]
      val root = XML.load(getClass.getResourceAsStream(_fileTypesXmlPath))
      val fileTypes = root \\ "filetype"
      for (fileType <- fileTypes) {
        val name = (fileType \ "@name").text
        val exts = (fileType \ "extensions").text.split("""\s+""").toSet
        fileTypeMap(name) = exts
      }
      fileTypeMap("text") = fileTypeMap("text") ++ fileTypeMap("code") ++
        fileTypeMap("xml")
      fileTypeMap("searchable") = fileTypeMap("text") ++ fileTypeMap("binary") ++
        fileTypeMap("compressed")
      _fileTypeMap ++= fileTypeMap
    }
    Map.empty[String, Set[String]] ++ _fileTypeMap
  }

  def getExtension(f: File): String = {
    val name = f.getName
    if (name.lastIndexOf('.') > 0 && name.lastIndexOf('.') < name.length-1)
      name.split('.').last
    else
      ""
  }

  def isBinaryFile(f: File) = {
    fileTypeMap("binary").contains(getExtension(f))
  }

  def isCompressedFile(f: File) = {
    fileTypeMap("compressed").contains(getExtension(f))
  }

  def isSearchableFile(f: File) = {
    fileTypeMap("searchable").contains(getExtension(f))
  }

  def isTextFile(f: File) = {
    fileTypeMap("text").contains(getExtension(f))
  }
}
