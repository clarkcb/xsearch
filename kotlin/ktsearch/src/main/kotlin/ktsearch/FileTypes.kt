package ktsearch

import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.parser.JSONParser
import org.json.simple.parser.ParseException
import org.w3c.dom.Element
import org.xml.sax.SAXException
import java.io.File
import java.io.IOException
import java.io.InputStreamReader
import java.util.*
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.ParserConfigurationException

/**
 * @author cary on 7/24/16.
 */

enum class FileType {
    UNKNOWN,
    ARCHIVE,
    BINARY,
    CODE,
    TEXT,
    XML
}

private val archive = "archive"
private val code = "code"
private val binary = "binary"
private val searchable = "searchable"
private val text = "text"
private val xml = "xml"
private val unknown = "unknown"

fun fromName(name: String) : FileType {
    val lname = name.trim().toLowerCase()
    when (lname) {
        text -> {
            return FileType.TEXT
        }
        binary -> {
            return FileType.BINARY
        }
        archive -> {
            return FileType.ARCHIVE
        }
        code -> {
            return FileType.CODE
        }
        xml -> {
            return FileType.XML
        }
        else -> {
            return FileType.UNKNOWN
        }
    }
}

class FileTypes {

    private val FILETYPESJSONPATH = "/filetypes.json"
    private val fileTypeMap: Map<String, Set<String>>

    init {
        fileTypeMap = getFileTypesFromJson()
    }

    private fun getFileTypesFromJson(): Map<String, Set<String>> {
        val ftMap: MutableMap<String, Set<String>> = mutableMapOf()
        val fileTypesInputStream = javaClass.getResourceAsStream(FILETYPESJSONPATH)

        try {
            val obj: Any = JSONParser().parse(InputStreamReader(fileTypesInputStream))
            val jsonObj = obj as JSONObject
            val filetypesArray = jsonObj["filetypes"] as JSONArray
            for (o in filetypesArray) {
                val filetypeMap = o as Map<*, *>
                val typeName = filetypeMap["type"] as String
                val extArray = filetypeMap["extensions"] as JSONArray
                val extSet: Set<String> = extArray.map { e -> e.toString() }.toSet()
                ftMap.put(typeName, extSet)
            }
            val allText: MutableSet<String> = mutableSetOf()
            allText.addAll(ftMap.get("code")!!)
            allText.addAll(ftMap.get("text")!!)
            allText.addAll(ftMap.get("xml")!!)
            ftMap.put("text",  allText)
            val allSearchable: MutableSet<String> = mutableSetOf()
            allSearchable.addAll(ftMap.get("archive")!!)
            allSearchable.addAll(ftMap.get("binary")!!)
            allSearchable.addAll(ftMap.get("text")!!)
            ftMap.put("searchable",  allSearchable)
        } catch (e: ParseException) {
            e.printStackTrace()
        } catch (e: IOException) {
            e.printStackTrace()
        }

        return ftMap
    }

    private fun getFileTypesFromXml(): Map<String, Set<String>> {
        val ftMap: MutableMap<String, Set<String>> = mutableMapOf()
        val fileTypesInputStream = javaClass.getResourceAsStream("/filetypes.xml")
        val factory = DocumentBuilderFactory.newInstance()

        try {
            val builder = factory.newDocumentBuilder()
            val doc = builder.parse(fileTypesInputStream)
            doc.documentElement.normalize()
            val filetypeNodes = doc.getElementsByTagName("filetype")
            for (i in 0..filetypeNodes.length - 1) {
                val fileTypeNode = filetypeNodes.item(i)
                val name = fileTypeNode.attributes.getNamedItem("name").nodeValue
                val extNode = (fileTypeNode as Element).getElementsByTagName("extensions").item(0)
                val extensions = extNode.childNodes.item(0).nodeValue
                val extSet = extensions.split("\\s+".toRegex()).filter { !it.isEmpty() }.toSet()
                ftMap.put(name, extSet)
            }
            val allText = (ftMap.get(code) ?: setOf()).
                    plus(ftMap.get(text) ?: setOf()).
                    plus(ftMap.get(xml) ?: setOf())
            ftMap.put(text, allText)
            val allSearchable = (ftMap.get(archive) ?: setOf()).
                    plus(ftMap.get(binary) ?: setOf()).
                    plus(ftMap.get(text) ?: setOf())
            ftMap.put(searchable, allSearchable)
        } catch (e: ParserConfigurationException) {
            e.printStackTrace()
        } catch (e: SAXException) {
            e.printStackTrace()
        } catch (e: IOException) {
            e.printStackTrace()
        }

        return ftMap
    }

    fun getFileType(file: File) : FileType {
        if (isTextFile(file)) {
            return FileType.TEXT
        } else if (isBinaryFile(file)) {
            return FileType.BINARY
        } else if (isArchiveFile(file)) {
            return FileType.ARCHIVE
        } else if (isCodeFile(file)) {
            return FileType.CODE
        } else if (isXmlFile(file)) {
            return FileType.XML
        } else {
            return FileType.UNKNOWN
        }
    }

    fun isArchiveFile(file: File): Boolean {
        return (fileTypeMap.get(archive) ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isBinaryFile(file: File): Boolean {
        return (fileTypeMap.get(binary) ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isCodeFile(file: File): Boolean {
        return (fileTypeMap.get(code) ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isSearchableFile(file: File): Boolean {
        return (fileTypeMap.get(searchable) ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isTextFile(file: File): Boolean {
        return (fileTypeMap.get(text) ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isUnknownFile(file: File): Boolean {
        return (fileTypeMap.get(unknown) ?: setOf()).contains(file.extension.toLowerCase())
                || !isSearchableFile(file)
    }

    fun isXmlFile(file: File): Boolean {
        return (fileTypeMap.get(xml) ?: setOf()).contains(file.extension.toLowerCase())
    }
}
