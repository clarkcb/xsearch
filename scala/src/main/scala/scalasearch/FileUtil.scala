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
    getExtension(f.fileName)
  }

  def getExtension(name: String): String = {
    val lastIndex = name.lastIndexOf('.')
    if (lastIndex > 0 && lastIndex < name.length-1)
      name.split('.').last
    else
      ""
  }

  def getFileType(f: File): FileType.Value = {
    if (isTextFile(f)) FileType.Text
    else if (isBinaryFile(f)) FileType.Binary
    else if (isArchiveFile(f)) FileType.Archive
    else FileType.Unknown
  }

  def getFileType(sf: SearchFile): FileType.Value = {
    if (isTextFile(sf)) FileType.Text
    else if (isBinaryFile(sf)) FileType.Binary
    else if (isArchiveFile(sf)) FileType.Archive
    else FileType.Unknown
  }

  def isHiddenFile(fileName: String): Boolean = {
    fileName.startsWith(".")
  }

  def isArchiveFile(f: File): Boolean = {
    isArchiveFile(f.getName)
  }

  def isArchiveFile(sf: SearchFile): Boolean = {
    isArchiveFile(sf.fileName)
  }

  def isArchiveFile(fileName: String): Boolean = {
    fileTypeMap("archive").contains(getExtension(fileName))
  }

  def isBinaryFile(f: File): Boolean = {
    isBinaryFile(f.getName)
  }

  def isBinaryFile(sf: SearchFile): Boolean = {
    isBinaryFile(sf.fileName)
  }

  def isBinaryFile(fileName: String): Boolean = {
    fileTypeMap("binary").contains(getExtension(fileName))
  }

  def isSearchableFile(f: File): Boolean = {
    isSearchableFile(f.getName)
  }

  def isSearchableFile(sf: SearchFile): Boolean = {
    isSearchableFile(sf.fileName)
  }

  def isSearchableFile(fileName: String): Boolean = {
    fileTypeMap("searchable").contains(getExtension(fileName))
  }

  def isTextFile(f: File): Boolean = {
    isTextFile(f.getName)
  }

  def isTextFile(sf: SearchFile): Boolean = {
    isTextFile(sf.fileName)
  }

  def isTextFile(fileName: String): Boolean = {
    fileTypeMap("text").contains(getExtension(fileName))
  }

  def isUnknownFile(f: File): Boolean = {
    isUnknownFile(f.getName)
  }

  def isUnknownFile(sf: SearchFile): Boolean = {
    isUnknownFile(sf.fileName)
  }

  def isUnknownFile(fileName: String): Boolean = {
    fileTypeMap("unknown").contains(getExtension(fileName)) ||
    !fileTypeMap("searchable").contains(getExtension(fileName))
  }

  def isZipArchiveFile(sf: SearchFile): Boolean = {
    Set("zip", "jar", "war").contains(getExtension(sf))
  }

  def isGzArchiveFile(sf: SearchFile): Boolean = {
    Set("gz", "tgz").contains(getExtension(sf))
  }

  def isBz2ArchiveFile(sf: SearchFile): Boolean = {
    "bz2" == getExtension(sf)
  }

  def isTarArchiveFile(sf: SearchFile): Boolean = {
    "tar" == getExtension(sf)
  }
}
