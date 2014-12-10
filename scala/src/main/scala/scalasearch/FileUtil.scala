package scalasearch

import java.io.File
import scala.collection.mutable
import scala.xml._

object FileType extends Enumeration {
  type FileType = Value
  val Unknown = Value
  val Archive = Value
  val Binary  = Value
  val Text    = Value
}

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
        fileTypeMap("archive")
      _fileTypeMap ++= fileTypeMap
    }
    Map.empty[String, Set[String]] ++ _fileTypeMap
  }

  def getExtension(f: File): String = {
    getExtension(f.getName)
  }

  def getExtension(f: SearchFile): String = {
    getExtension(f.file)
  }

  def getExtension(name: String): String = {
    if (name.lastIndexOf('.') > 0 && name.lastIndexOf('.') < name.length-1)
      name.split('.').last
    else
      ""
  }

  def getFileType(f: File): FileType.Value = {
    if (isArchiveFile(f)) FileType.Archive
    else if (isBinaryFile(f)) FileType.Binary
    else if (isTextFile(f)) FileType.Text
    else FileType.Unknown
  }

  def getFileType(sf: SearchFile): FileType.Value = {
    if (isArchiveFile(sf)) FileType.Archive
    else if (isBinaryFile(sf)) FileType.Binary
    else if (isTextFile(sf)) FileType.Text
    else FileType.Unknown
  }

  def isArchiveFile(f: File): Boolean = {
    fileTypeMap("archive").contains(getExtension(f))
  }

  def isArchiveFile(sf: SearchFile): Boolean = {
    fileTypeMap("archive").contains(getExtension(sf))
  }

  def isBinaryFile(f: File): Boolean = {
    fileTypeMap("binary").contains(getExtension(f))
  }

  def isBinaryFile(sf: SearchFile): Boolean = {
    fileTypeMap("binary").contains(getExtension(sf))
  }

  def isSearchableFile(f: File): Boolean = {
    fileTypeMap("searchable").contains(getExtension(f))
  }

  def isSearchableFile(sf: SearchFile): Boolean = {
    fileTypeMap("searchable").contains(getExtension(sf))
  }

  def isTextFile(f: File): Boolean = {
    fileTypeMap("text").contains(getExtension(f))
  }

  def isTextFile(sf: SearchFile): Boolean = {
    fileTypeMap("text").contains(getExtension(sf))
  }

  def isUnknownFile(f: File): Boolean = {
    fileTypeMap("unknown").contains(getExtension(f)) ||
    !fileTypeMap("searchable").contains(getExtension(f))
  }

  def isUnknownFile(sf: SearchFile): Boolean = {
    fileTypeMap("unknown").contains(getExtension(sf)) ||
    !fileTypeMap("searchable").contains(getExtension(sf))
  }
}
