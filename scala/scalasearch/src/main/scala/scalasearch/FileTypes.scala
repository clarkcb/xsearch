package scalasearch

import java.io.{File, IOException, InputStreamReader}

import org.json.simple.parser.{JSONParser, ParseException}
import org.json.simple.{JSONArray, JSONObject}

import scala.collection.mutable
import scala.xml.XML

object FileType extends Enumeration {
  type FileType = Value
  val Unknown: FileType = Value
  val Archive: FileType = Value
  val Binary: FileType  = Value
  val Code: FileType    = Value
  val Text: FileType    = Value
  val Xml: FileType     = Value
}

object FileTypes {
  private val _fileTypesJsonPath = "/filetypes.json"
  private val _fileTypesXmlPath = "/filetypes.xml"
  private val _fileTypeMap = mutable.Map.empty[String, Set[String]]

  private val archive = "archive"
  private val binary = "binary"
  private val code = "code"
  private val searchable = "searchable"
  private val text = "text"
  private val unknown = "unknown"
  private val xml = "xml"

  private def fileTypeMap: Map[String, Set[String]] = {
    if (_fileTypeMap.isEmpty) {
      val fileTypesInputStream = getClass.getResourceAsStream(_fileTypesJsonPath)
      try {
        val obj = new JSONParser().parse(new InputStreamReader(fileTypesInputStream))
        val jsonObj = obj.asInstanceOf[JSONObject]
        val ftIt = jsonObj.get("filetypes").asInstanceOf[JSONArray].iterator()
        while (ftIt.hasNext) {
          val ftObj = ftIt.next().asInstanceOf[JSONObject]
          val typeName = ftObj.get("type").asInstanceOf[String]
          val exSet = mutable.Set.empty[String]
          val exIt = ftObj.get("extensions").asInstanceOf[JSONArray].iterator()
          while (exIt.hasNext) {
             exSet += exIt.next().asInstanceOf[String]
          }
          _fileTypeMap.put(typeName, Set.empty[String] ++ exSet)
        }
      } catch {
        case e: ParseException =>
          print(e.getMessage)
        case e: IOException =>
          print(e.getMessage)
      }

      _fileTypeMap(text) = _fileTypeMap(text) ++ _fileTypeMap(code) ++
        _fileTypeMap(xml)
      _fileTypeMap(searchable) = _fileTypeMap(text) ++ _fileTypeMap(binary) ++
        _fileTypeMap(archive)
      Map.empty[String, Set[String]] ++ _fileTypeMap
    } else {
      Map.empty[String, Set[String]] ++ _fileTypeMap
    }
  }

  private def fileTypeMapFromXml: Map[String, Set[String]] = {
    if (_fileTypeMap.isEmpty) {
      val fileTypeMap = mutable.Map.empty[String, Set[String]]
      val root = XML.load(getClass.getResourceAsStream(_fileTypesXmlPath))
      val fileTypes = root \\ "filetype"
      for (fileType <- fileTypes) {
        val name = (fileType \ "@name").text
        val exts = (fileType \ "extensions").text.split("""\s+""").toSet
        fileTypeMap(name) = exts
      }
      fileTypeMap(text) = fileTypeMap(text) ++ fileTypeMap(code) ++
        fileTypeMap(xml)
      fileTypeMap(searchable) = fileTypeMap(text) ++ fileTypeMap(binary) ++
        fileTypeMap(archive)
      _fileTypeMap ++= fileTypeMap
    }
    Map.empty[String, Set[String]] ++ _fileTypeMap
  }

  def fromName(name: String): FileType.Value = {
    val lname = name.toLowerCase
    if (lname == text) {
      FileType.Text
    } else if (lname == binary) {
      FileType.Binary
    } else if (lname == archive) {
      FileType.Archive
    } else if (lname == code) {
      FileType.Code
    } else if (lname == xml) {
      FileType.Xml
    } else {
      FileType.Unknown
    }
  }

  def getFileType(f: File): FileType.Value = {
    if (isCodeFile(f)) {
      FileType.Code
    } else if (isXmlFile(f)) {
      FileType.Xml
    } else if (isTextFile(f)) {
      FileType.Text
    } else if (isBinaryFile(f)) {
      FileType.Binary
    } else if (isArchiveFile(f)) {
      FileType.Archive
    } else {
      FileType.Unknown
    }
  }

  def getFileType(f: SearchFile): FileType.Value = {
    getFileType(f.toFile)
  }

  def isArchiveFile(f: File): Boolean = {
    isArchiveFile(f.getName)
  }

  def isArchiveFile(sf: SearchFile): Boolean = {
    isArchiveFile(sf.fileName)
  }

  def isArchiveFile(fileName: String): Boolean = {
    fileTypeMap(archive).contains(FileUtil.getExtension(fileName))
  }

  def isBinaryFile(f: File): Boolean = {
    isBinaryFile(f.getName)
  }

  def isBinaryFile(sf: SearchFile): Boolean = {
    isBinaryFile(sf.fileName)
  }

  def isBinaryFile(fileName: String): Boolean = {
    fileTypeMap(binary).contains(FileUtil.getExtension(fileName))
  }

  def isCodeFile(f: File): Boolean = {
    isCodeFile(f.getName)
  }

  def isCodeFile(sf: SearchFile): Boolean = {
    isCodeFile(sf.fileName)
  }

  def isCodeFile(fileName: String): Boolean = {
    fileTypeMap(code).contains(FileUtil.getExtension(fileName))
  }

  def isSearchableFile(f: File): Boolean = {
    isSearchableFile(f.getName)
  }

  def isSearchableFile(sf: SearchFile): Boolean = {
    isSearchableFile(sf.fileName)
  }

  def isSearchableFile(fileName: String): Boolean = {
    fileTypeMap(searchable).contains(FileUtil.getExtension(fileName))
  }

  def isTextFile(f: File): Boolean = {
    isTextFile(f.getName)
  }

  def isTextFile(sf: SearchFile): Boolean = {
    isTextFile(sf.fileName)
  }

  def isTextFile(fileName: String): Boolean = {
    fileTypeMap(text).contains(FileUtil.getExtension(fileName))
  }

  def isUnknownFile(f: File): Boolean = {
    isUnknownFile(f.getName)
  }

  def isUnknownFile(sf: SearchFile): Boolean = {
    isUnknownFile(sf.fileName)
  }

  def isUnknownFile(fileName: String): Boolean = {
    fileTypeMap(unknown).contains(FileUtil.getExtension(fileName)) ||
      !fileTypeMap(searchable).contains(FileUtil.getExtension(fileName))
  }

  def isXmlFile(f: File): Boolean = {
    isXmlFile(f.getName)
  }

  def isXmlFile(sf: SearchFile): Boolean = {
    isXmlFile(sf.fileName)
  }

  def isXmlFile(fileName: String): Boolean = {
    fileTypeMap(xml).contains(FileUtil.getExtension(fileName))
  }

  def isZipArchiveFile(sf: SearchFile): Boolean = {
    Set("zip", "jar", "war").contains(FileUtil.getExtension(sf))
  }

  def isGzArchiveFile(sf: SearchFile): Boolean = {
    Set("gz", "tgz").contains(FileUtil.getExtension(sf))
  }

  def isBz2ArchiveFile(sf: SearchFile): Boolean = {
    "bz2" == FileUtil.getExtension(sf)
  }

  def isTarArchiveFile(sf: SearchFile): Boolean = {
    "tar" == FileUtil.getExtension(sf)
  }
}
