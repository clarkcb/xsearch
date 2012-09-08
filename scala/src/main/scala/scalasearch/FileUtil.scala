package scalasearch

import java.io.File
import scala.collection.mutable
import scala.xml._

class FileUtil {
  // TODO: move to config file
  private val fileTypePath = "/Users/cary/src/git/xsearch/shared/filetypes.xml"
  private val fileTypeMap = getFileTypeMap

  def getFileTypeMap: mutable.Map[String, Set[String]] = {
    val fileTypeMap = mutable.Map.empty[String, Set[String]]
    val root = XML.loadFile(fileTypePath)
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
    fileTypeMap
  }

  def getExtension(f: File) = {
    f.getName.split('.').last
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
