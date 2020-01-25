package ktsearch

import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.JSONValue
import org.json.simple.parser.JSONParser
import org.json.simple.parser.ParseException
import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStreamReader
import javax.xml.parsers.DocumentBuilderFactory

/**
 * @author cary on 7/23/16.
 */
data class SearchOption(val shortarg: String, val longarg: String, val desc: String) {
    val sortarg =
            if (shortarg.isNotEmpty()) {
                shortarg.toLowerCase() + "@" + longarg.toLowerCase()
            } else {
                longarg.toLowerCase()
            }
}

class SearchOptions() {
    val searchOptions : List<SearchOption>

    init {
        searchOptions = loadSearchOptionsFromJson()
    }

    private fun loadSearchOptionsFromJson() : List<SearchOption> {
        val searchOptionsXmlPath = "/searchoptions.json"
        val searchOptionsInputStream = javaClass.getResourceAsStream(searchOptionsXmlPath)
        val obj: Any = JSONParser().parse(InputStreamReader(searchOptionsInputStream))
        val jsonObj = obj as JSONObject
        val searchoptionsArray = jsonObj["searchoptions"] as JSONArray

        val options : MutableList<SearchOption> = mutableListOf()
        for (o in searchoptionsArray) {
            val searchoptionMap = o as Map<String, String>
            val longArg = searchoptionMap["long"] as String
            val desc = searchoptionMap["desc"] as String
            var shortArg = ""
            if (searchoptionMap.containsKey("short")) {
                shortArg = searchoptionMap["short"] as String
            }
            options.add(SearchOption(shortArg, longArg, desc))
        }
        return options.toList().sortedBy { it.sortarg }
    }

    private fun loadSearchOptionsFromXml() : List<SearchOption> {
        val searchOptionsXmlPath = "/searchoptions.xml"
        val searchOptionsInputStream = javaClass.getResourceAsStream(searchOptionsXmlPath)
        val factory = DocumentBuilderFactory.newInstance()
        val builder = factory.newDocumentBuilder()
        val doc = builder.parse(searchOptionsInputStream)
        doc.getDocumentElement().normalize()
        val searchOptionNodes = doc.getElementsByTagName("searchoption")
        val options : MutableList<SearchOption> = mutableListOf()
        for (i in 0..(searchOptionNodes.length - 1)) {
            val searchOptionNode = searchOptionNodes.item(i)
            val longArg = searchOptionNode.getAttributes().getNamedItem("long").getNodeValue()
            val shortArg = searchOptionNode.getAttributes().getNamedItem("short").getNodeValue()
            val desc = searchOptionNode.getTextContent().trim()
            options.add(SearchOption(shortArg, longArg, desc))
        }
        return options.toList().sortedBy { it.sortarg }
    }

    private fun getArgMap() : Map<String, String> {
        val longOpts = searchOptions.map { Pair(it.longarg, it.longarg) }.toMap()
        val shortOpts = searchOptions.filter { it.shortarg.isNotEmpty() }.map { Pair(it.shortarg, it.longarg) }.toMap()
        return longOpts.plus(shortOpts)
    }

    private val argActionMap: Map<String, ((String, SearchSettings) -> SearchSettings)> = mapOf(
            "encoding" to
                    { s, ss -> ss.copy(textFileEncoding = s) },
            "in-archiveext" to
                    { s, ss -> ss.copy(inArchiveExtensions = addExtensions(s, ss.inArchiveExtensions)) },
            "in-archivefilepattern" to
                    { s, ss -> ss.copy(inArchiveFilePatterns = ss.inArchiveFilePatterns.plus(Regex(s))) },
            "in-dirpattern" to
                    { s, ss -> ss.copy(inDirPatterns = ss.inDirPatterns.plus(Regex(s))) },
            "in-ext" to
                    { s, ss -> ss.copy(inExtensions = addExtensions(s, ss.inExtensions)) },
            "in-filepattern" to
                    { s, ss -> ss.copy(inFilePatterns = ss.inFilePatterns.plus(Regex(s))) },
            "in-filetype" to
                    { s, ss -> ss.copy(inFileTypes = addFileTypes(s, ss.inFileTypes)) },
            "in-linesafterpattern" to
                    { s, ss -> ss.copy(inLinesAfterPatterns = ss.inLinesAfterPatterns.plus(Regex(s))) },
            "in-linesbeforepattern" to
                    { s, ss -> ss.copy(inLinesBeforePatterns = ss.inLinesBeforePatterns.plus(Regex(s))) },
            "linesafter" to
                    { s, ss -> ss.copy(linesAfter = s.toInt()) },
            "linesaftertopattern" to
                    { s, ss -> ss.copy(linesAfterToPatterns = ss.linesAfterToPatterns.plus(Regex(s))) },
            "linesafteruntilpattern" to
                    { s, ss -> ss.copy(linesAfterUntilPatterns = ss.linesAfterUntilPatterns.plus(Regex(s))) },
            "linesbefore" to
                    { s, ss -> ss.copy(linesBefore = s.toInt()) },
            "maxlinelength" to
                    { s, ss -> ss.copy(maxLineLength = s.toInt()) },
            "out-archiveext" to
                    { s, ss -> ss.copy(outArchiveExtensions = addExtensions(s, ss.outArchiveExtensions)) },
            "out-archivefilepattern" to
                    { s, ss -> ss.copy(outArchiveFilePatterns = ss.outArchiveFilePatterns.plus(Regex(s))) },
            "out-dirpattern" to
                    { s, ss -> ss.copy(outDirPatterns = ss.outDirPatterns.plus(Regex(s))) },
            "out-ext" to
                    { s, ss -> ss.copy(outExtensions = addExtensions(s, ss.outExtensions)) },
            "out-filepattern" to
                    { s, ss -> ss.copy(outFilePatterns = ss.outFilePatterns.plus(Regex(s))) },
            "out-filetype" to
                    { s, ss -> ss.copy(outFileTypes = addFileTypes(s, ss.outFileTypes)) },
            "out-linesafterpattern" to
                    { s, ss -> ss.copy(outLinesAfterPatterns = ss.outLinesAfterPatterns.plus(Regex(s))) },
            "out-linesbeforepattern" to
                    { s, ss -> ss.copy(outLinesBeforePatterns = ss.outLinesBeforePatterns.plus(Regex(s))) },
            "searchpattern" to
                    { s, ss -> ss.copy(searchPatterns = ss.searchPatterns.plus(Regex(s))) },
            "settings-file" to
                    { s, ss -> settingsFromFile(s, ss) }
    )

    private val boolFlagActionMap: Map<String, ((Boolean, SearchSettings) -> SearchSettings)> = mapOf(
            "archivesonly" to { b, ss -> if (b) ss.copy(archivesOnly = b,
                    searchArchives = b) else ss.copy(archivesOnly = b) },
            "allmatches" to { b, ss -> ss.copy(firstMatch = !b) },
            "debug" to { b, ss -> if (b) ss.copy(debug = b, verbose = b) else
                ss.copy(debug = b) },
            "excludehidden" to { b, ss -> ss.copy(excludeHidden = b) },
            "firstmatch" to { b, ss -> ss.copy(firstMatch = b) },
            "help" to { b, ss -> ss.copy(printUsage = b) },
            "includehidden" to { b, ss -> ss.copy(excludeHidden = !b) },
            "listdirs" to { b, ss -> ss.copy(listDirs = b) },
            "listfiles" to { b, ss -> ss.copy(listFiles = b) },
            "listlines" to { b, ss -> ss.copy(listLines = b) },
            "multilinesearch" to { b, ss -> ss.copy(multiLineSearch = b) },
            "noprintmatches" to { b, ss -> ss.copy(printResults = !b) },
            "norecursive" to { b, ss -> ss.copy(recursive = !b) },
            "nosearcharchives" to { b, ss -> ss.copy(searchArchives = !b) },
            "printmatches" to { b, ss -> ss.copy(printResults = b) },
            "recursive" to { b, ss -> ss.copy(recursive = b) },
            "searcharchives" to { b, ss -> ss.copy(searchArchives = b) },
            "uniquelines" to { b, ss -> ss.copy(uniqueLines = b) },
            "verbose" to { b, ss -> ss.copy(verbose = b) },
            "version" to { b, ss -> ss.copy(printVersion = b) }
    )

    private fun settingsFromFile(filePath: String, settings: SearchSettings) : SearchSettings {
        val file = File(filePath)
        try {
            val json = file.readText()
            return settingsFromJson(json, settings)
        } catch (e: FileNotFoundException) {
            throw SearchException("Settings file not found: " + filePath)
        } catch (e: IOException) {
            throw SearchException("IOException reading settings file: " + filePath)
        } catch (e: ParseException) {
            throw SearchException("ParseException trying to parse the JSON in " + filePath)
        }
    }

    fun settingsFromJson(json: String, settings: SearchSettings): SearchSettings {
        val obj = JSONValue.parseWithException(json)
        val jsonObject = obj as JSONObject
        fun recSettingsFromJson(keys: Set<Any?>, settings: SearchSettings) : SearchSettings {
            if (keys.isEmpty()) return settings
            else {
                val ko = keys.first()
                val vo = jsonObject.get(ko)
                if (ko != null && ko is String && vo != null) {
                    return recSettingsFromJson(keys.minus(ko), applySetting(ko, vo, settings))
                } else {
                    return recSettingsFromJson(keys.minus(ko), settings)
                }
            }
        }
        return recSettingsFromJson(obj.keys, settings)
    }

    fun applySetting(key: String, obj: Any, settings: SearchSettings): SearchSettings {
        if (obj is String) {
            return applySetting(key, obj, settings)
        } else if (obj is Boolean) {
            return applySetting(key, obj, settings)
        } else if (obj is Long) {
            return applySetting(key, obj.toString(), settings)
        } else if (obj is JSONArray) {
            return applySetting(key, obj.toList().map { it as String }, settings)
        } else {
            return settings
        }
    }

    fun applySetting(key: String, s: String, settings: SearchSettings): SearchSettings {
        if (this.argActionMap.containsKey(key)) {
            return this.argActionMap.get(key)!!.invoke(s, settings)
        } else if (key == "startpath") {
            return settings.copy(startPath = s)
        } else {
            throw SearchException("Invalid option: " + key)
        }
    }

    fun applySetting(key: String, bool: Boolean, settings: SearchSettings): SearchSettings {
        if (this.boolFlagActionMap.containsKey(key)) {
            return this.boolFlagActionMap.get(key)!!.invoke(bool, settings)
        } else {
            throw SearchException("Invalid option: " + key)
        }
    }

    fun applySetting(key: String, lst: List<String>, settings: SearchSettings): SearchSettings {
        if (lst.isEmpty()) return settings
        else {
            return applySetting(key, lst.drop(1), applySetting(key, lst.first(), settings))
        }
    }

    fun settingsFromArgs(args : Array<String>) : SearchSettings {
        val argMap = getArgMap()
        fun recSettingsFromArgs(args: List<String>, settings: SearchSettings) : SearchSettings {
            if (args.isEmpty()) return settings
            val nextArg = args.first()
            if (nextArg.startsWith("-")) {
                val arg = nextArg.dropWhile { it == '-' }
                if (argMap.containsKey(arg)) {
                    val longArg = argMap.get(arg)
                    if (argActionMap.containsKey(longArg)) {
                        if (args.size > 1) {
                            val argVal = args.drop(1).first()
                            val ss = argActionMap.get(longArg)!!.invoke(argVal, settings)
                            return recSettingsFromArgs(args.drop(2), ss)
                        } else {
                            throw SearchException("Missing value for option " + arg)
                        }
                    } else if (boolFlagActionMap.containsKey(longArg)) {
                        val ss = boolFlagActionMap.get(longArg)!!.invoke(true, settings)
                        return recSettingsFromArgs(args.drop(1), ss)
                    } else {
                        throw SearchException("Invalid option: " + arg)
                    }
                } else {
                    throw SearchException("Invalid option: " + arg)
                }
            } else {
                return recSettingsFromArgs(args.drop(1), settings.copy(startPath = nextArg))
            }
        }
        return recSettingsFromArgs(args.toList(), getDefaultSettings().copy(printResults = true))
    }

    fun usage() {
        log(getUsageString())
    }

    fun getUsageString() : String {
        val sb = StringBuilder()
        sb.append("Usage:\n")
        sb.append(" ktsearch [options] -s <searchpattern> <startpath>\n\n")
        sb.append("Options:\n")
        fun getOptString(so: SearchOption): String {
            val s = if (so.shortarg.isEmpty()) "" else "-${so.shortarg},"
            return s + "--${so.longarg}"
        }
        val optStrings = searchOptions.map { getOptString(it) }
        val optDescs = searchOptions.map { it.desc }
        val longest = optStrings.map { it.length }.max()
        val format = " %1${'$'}-${longest}s  %2${'$'}s\n"
        for (i in optStrings.indices) {
            sb.append(String.format(format, optStrings[i], optDescs[i]))
        }
        return sb.toString()
    }
}
