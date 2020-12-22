package ktsearch

import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.parser.JSONParser
import org.json.simple.parser.ParseException
import java.io.File
import java.io.IOException
import java.io.InputStreamReader

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

private const val archive = "archive"
private const val code = "code"
private const val binary = "binary"
private const val searchable = "searchable"
private const val text = "text"
private const val xml = "xml"
private const val unknown = "unknown"

fun fromName(name: String) : FileType {
    when (name.trim().toLowerCase()) {
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

private const val fileTypesJsonPath = "/filetypes.json"

class FileTypes {

    private val fileTypeMap: Map<String, Set<String>>

    init {
        fileTypeMap = getFileTypesFromJson()
    }

    private fun getFileTypesFromJson(): Map<String, Set<String>> {
        val ftMap: MutableMap<String, Set<String>> = mutableMapOf()
        val fileTypesInputStream = javaClass.getResourceAsStream(fileTypesJsonPath)

        try {
            val obj: Any = JSONParser().parse(InputStreamReader(fileTypesInputStream))
            val jsonObj = obj as JSONObject
            val filetypesArray = jsonObj["filetypes"] as JSONArray
            for (o in filetypesArray) {
                val filetypeMap = o as Map<*, *>
                val typeName = filetypeMap["type"] as String
                val extArray = filetypeMap["extensions"] as JSONArray
                val extSet: Set<String> = extArray.map { e -> e.toString() }.toSet()
                ftMap[typeName] = extSet
            }
            val allText: MutableSet<String> = mutableSetOf()
            allText.addAll(ftMap["code"]!!)
            allText.addAll(ftMap["text"]!!)
            allText.addAll(ftMap["xml"]!!)
            ftMap["text"] = allText
            val allSearchable: MutableSet<String> = mutableSetOf()
            allSearchable.addAll(ftMap["archive"]!!)
            allSearchable.addAll(ftMap["binary"]!!)
            allSearchable.addAll(ftMap["text"]!!)
            ftMap["searchable"] = allSearchable
        } catch (e: ParseException) {
            e.printStackTrace()
        } catch (e: IOException) {
            e.printStackTrace()
        }

        return ftMap
    }

    fun getFileType(file: File) : FileType {
        when {
            isTextFile(file) -> {
                return FileType.TEXT
            }
            isBinaryFile(file) -> {
                return FileType.BINARY
            }
            isArchiveFile(file) -> {
                return FileType.ARCHIVE
            }
            isCodeFile(file) -> {
                return FileType.CODE
            }
            isXmlFile(file) -> {
                return FileType.XML
            }
            else -> {
                return FileType.UNKNOWN
            }
        }
    }

    fun isArchiveFile(file: File): Boolean {
        return (fileTypeMap[archive] ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isBinaryFile(file: File): Boolean {
        return (fileTypeMap[binary] ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isCodeFile(file: File): Boolean {
        return (fileTypeMap[code] ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isSearchableFile(file: File): Boolean {
        return (fileTypeMap[searchable] ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isTextFile(file: File): Boolean {
        return (fileTypeMap[text] ?: setOf()).contains(file.extension.toLowerCase())
    }

    fun isUnknownFile(file: File): Boolean {
        return (fileTypeMap[unknown] ?: setOf()).contains(file.extension.toLowerCase())
                || !isSearchableFile(file)
    }

    fun isXmlFile(file: File): Boolean {
        return (fileTypeMap[xml] ?: setOf()).contains(file.extension.toLowerCase())
    }
}
