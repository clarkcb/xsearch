package ktsearch

import com.beust.klaxon.JsonArray
import com.beust.klaxon.JsonObject
import com.beust.klaxon.KlaxonException
import com.beust.klaxon.Parser
import ktfind.getLastModFromString
import ktfind.sortByFromName
import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStreamReader

/**
 * @author cary on 7/23/16.
 */
data class SearchOption(val shortarg: String?, val longarg: String, val desc: String) {
    val sortarg =
            if (shortarg == null) {
                longarg.lowercase()
            } else {
                shortarg.lowercase() + "@" + longarg.lowercase()
            }
}

class SearchOptions {
    private val searchOptions : List<SearchOption>

    init {
        searchOptions = loadSearchOptionsFromJson()
    }

    private fun loadSearchOptionsFromJson() : List<SearchOption> {
        val searchOptionsJsonPath = "/searchoptions.json"
        val searchOptionsInputStream = javaClass.getResourceAsStream(searchOptionsJsonPath)
        val jsonObj: JsonObject = Parser.default().parse(InputStreamReader(searchOptionsInputStream!!)) as JsonObject
        val searchOptionsArray = jsonObj.array<JsonObject>("searchoptions")

        val options : MutableList<SearchOption> = mutableListOf()
        searchOptionsArray!!.forEach {
            val longArg = it.string("long")!!
            val desc = it.string("desc")!!
            val shortArg: String? = it.string("short")
            options.add(SearchOption(shortArg, longArg, desc))
        }
        return options.toList().sortedBy { it.sortarg }
    }

    private fun getArgMap() : Map<String, String> {
        val longOpts = searchOptions.map { Pair(it.longarg, it.longarg) }.toMap()
        val shortOpts = searchOptions.filter { it.shortarg != null }.map { Pair(it.shortarg!!, it.longarg) }.toMap()
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
        "maxlastmod" to
                { s, ss -> ss.copy(maxLastMod = getLastModFromString(s)) },
        "maxlinelength" to
                { s, ss -> ss.copy(maxLineLength = s.toInt()) },
        "maxsize" to
                { s, ss -> ss.copy(maxSize = Integer.parseInt(s)) },
        "minlastmod" to
                { s, ss -> ss.copy(minLastMod = getLastModFromString(s)) },
        "minsize" to
                { s, ss -> ss.copy(minSize = Integer.parseInt(s)) },
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
                { s, ss -> settingsFromFile(s, ss) },
        "sort-by" to
                { s, ss -> ss.copy(sortBy = sortByFromName(s)) },
    )

    private val boolFlagActionMap: Map<String, ((Boolean, SearchSettings) -> SearchSettings)> = mapOf(
        "archivesonly" to { b, ss -> if (b) ss.copy(archivesOnly = b,
            searchArchives = b) else ss.copy(archivesOnly = b) },
        "allmatches" to { b, ss -> ss.copy(firstMatch = !b) },
        "colorize" to { b, ss -> ss.copy(colorize = b) },
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
        "sort-ascending" to { b, ss -> ss.copy(sortDescending = !b) },
        "sort-caseinsensitive" to { b, ss -> ss.copy(sortCaseInsensitive = b) },
        "sort-casesensitive" to { b, ss -> ss.copy(sortCaseInsensitive = !b) },
        "sort-descending" to { b, ss -> ss.copy(sortDescending = b) },
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
            throw SearchException("Settings file not found: $filePath")
        } catch (e: IOException) {
            throw SearchException("IOException reading settings file: $filePath")
        } catch (e: KlaxonException) {
            throw SearchException("KlaxonException trying to parse the JSON in $filePath")
        }
    }

    fun settingsFromJson(json: String, settings: SearchSettings): SearchSettings {
        val jsonObject = Parser.default().parse(StringBuilder(json)) as JsonObject
        fun recSettingsFromJson(keys: List<Any?>, settings: SearchSettings) : SearchSettings {
            return if (keys.isEmpty()) settings
            else {
                val ko = keys.first()
                val vo = jsonObject[ko]
                if (ko != null && ko is String && vo != null) {
                    recSettingsFromJson(keys.drop(1), applySetting(ko, vo, settings))
                } else {
                    recSettingsFromJson(keys.drop(1), settings)
                }
            }
        }
        return recSettingsFromJson(jsonObject.keys.toList(), settings)
    }

    private fun applySetting(key: String, obj: Any, settings: SearchSettings): SearchSettings {
        when (obj) {
            is String -> {
                return applySetting(key, obj, settings)
            }
            is Boolean -> {
                return applySetting(key, obj, settings)
            }
            is Long -> {
                return applySetting(key, obj.toString(), settings)
            }
            is JsonArray<*> -> {
                return applySetting(key, obj.toList().map { it as String }, settings)
            }
            else -> {
                return settings
            }
        }
    }

    private fun applySetting(key: String, s: String, settings: SearchSettings): SearchSettings {
        return when {
            this.argActionMap.containsKey(key) -> {
                this.argActionMap[key]!!.invoke(s, settings)
            }
            key == "path" -> {
                settings.copy(paths = settings.paths.plus(s))
            }
            else -> {
                throw SearchException("Invalid option: $key")
            }
        }
    }

    private fun applySetting(key: String, bool: Boolean, settings: SearchSettings): SearchSettings {
        if (this.boolFlagActionMap.containsKey(key)) {
            return this.boolFlagActionMap[key]!!.invoke(bool, settings)
        } else {
            throw SearchException("Invalid option: $key")
        }
    }

    private fun applySetting(key: String, lst: List<String>, settings: SearchSettings): SearchSettings {
        return if (lst.isEmpty()) settings
        else {
            applySetting(key, lst.drop(1), applySetting(key, lst.first(), settings))
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
                    val longArg = argMap[arg]
                    return if (argActionMap.containsKey(longArg)) {
                        if (args.size > 1) {
                            val argVal = args.drop(1).first()
                            val ss = argActionMap[longArg]!!.invoke(argVal, settings)
                            recSettingsFromArgs(args.drop(2), ss)
                        } else {
                            throw SearchException("Missing value for option $arg")
                        }
                    } else if (boolFlagActionMap.containsKey(longArg)) {
                        val ss = boolFlagActionMap[longArg]!!.invoke(true, settings)
                        recSettingsFromArgs(args.drop(1), ss)
                    } else {
                        throw SearchException("Invalid option: $arg")
                    }
                } else {
                    throw SearchException("Invalid option: $arg")
                }
            } else {
                return recSettingsFromArgs(args.drop(1), settings.copy(paths = settings.paths.plus(nextArg)))
            }
        }
        return recSettingsFromArgs(args.toList(), getDefaultSettings().copy(printResults = true))
    }

    fun usage() {
        log(getUsageString())
    }

    private fun getUsageString() : String {
        val sb = StringBuilder()
        sb.append("Usage:\n")
        sb.append(" ktsearch [options] -s <searchpattern> <path> [<path> ...]\n\n")
        sb.append("Options:\n")
        fun getOptString(so: SearchOption): String {
            return (if (so.shortarg == null) "" else "-${so.shortarg},") + "--${so.longarg}"
        }
        val optPairs = searchOptions.map { Pair(getOptString(it), it.desc) }
        val longest = optPairs.map { it.first.length }.maxOrNull()
        val format = " %1${'$'}-${longest}s  %2${'$'}s\n"
        for (o in optPairs) {
            sb.append(String.format(format, o.first, o.second))
        }
        return sb.toString()
    }
}
