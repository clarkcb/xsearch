package ktsearch

import ktfind.*
import org.json.JSONArray
import org.json.JSONException
import org.json.JSONObject
import org.json.JSONTokener
import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths

/**
 * @author cary on 7/23/16.
 */
data class SearchOption(val shortArg: String?, val longArg: String, val desc: String) {
    val sortArg =
            if (shortArg == null) {
                longArg.lowercase()
            } else {
                shortArg.lowercase() + "@" + longArg.lowercase()
            }
}

class SearchOptions {
    private val searchOptionsJsonPath = "/searchoptions.json"
    private val searchOptions : List<SearchOption>
    // We add path manually since it's not an option in searchoptions.json
    private var longArgMap = mutableMapOf<String, String>("path" to "path")

    init {
        searchOptions = loadSearchOptionsFromJson()
    }

    private fun loadSearchOptionsFromJson() : List<SearchOption> {
        val searchOptionsInputStream = javaClass.getResourceAsStream(searchOptionsJsonPath)
        val jsonObj = JSONObject(JSONTokener(searchOptionsInputStream))
        val searchOptionsArray = jsonObj.getJSONArray("searchoptions").iterator()
        val options : MutableList<SearchOption> = mutableListOf()
        while (searchOptionsArray.hasNext()) {
            val searchOptionObj = searchOptionsArray.next() as JSONObject
            val longArg = searchOptionObj.getString("long")
            longArgMap[longArg] = longArg
            val shortArg =
                if (searchOptionObj.has("short")) {
                    val sArg = searchOptionObj.getString("short")
                    longArgMap[sArg] = longArg
                    sArg
                } else {
                    null
                }
            val desc = searchOptionObj.getString("desc")
            options.add(SearchOption(shortArg, longArg, desc))
        }
        return options.toList().sortedBy { it.sortArg }
    }

    private val boolActionMap: Map<String, ((Boolean, SearchSettings) -> SearchSettings)> = mapOf(
        "archivesonly" to { b, ss ->
            if (b) ss.copy(
                archivesOnly = b,
                searchArchives = b
            ) else ss.copy(archivesOnly = b) },
        "allmatches" to { b, ss -> ss.copy(firstMatch = !b) },
        "colorize" to { b, ss -> ss.copy(colorize = b) },
        "debug" to { b, ss ->
            if (b) ss.copy(debug = b, verbose = b) else
                ss.copy(debug = b)
        },
        "excludehidden" to { b, ss -> ss.copy(includeHidden = !b) },
        "firstmatch" to { b, ss -> ss.copy(firstMatch = b) },
        "followsymlinks" to { b, ss -> ss.copy(followSymlinks = b) },
        "help" to { b, ss -> ss.copy(printUsage = b) },
        "includehidden" to { b, ss -> ss.copy(includeHidden = b) },
        "multilinesearch" to { b, ss -> ss.copy(multiLineSearch = b) },
        "nocolorize" to { b, ss -> ss.copy(colorize = !b) },
        "nofollowsymlinks" to { b, ss -> ss.copy(followSymlinks = !b) },
        "noprintdirs" to { b, ss -> ss.copy(printDirs = !b) },
        "noprintfiles" to { b, ss -> ss.copy(printFiles = !b) },
        "noprintlines" to { b, ss -> ss.copy(printLines = !b) },
        "noprintmatches" to { b, ss -> ss.copy(printResults = !b) },
        "norecursive" to { b, ss -> ss.copy(recursive = !b) },
        "nosearcharchives" to { b, ss -> ss.copy(searchArchives = !b) },
        "printdirs" to { b, ss -> ss.copy(printDirs = b) },
        "printfiles" to { b, ss -> ss.copy(printFiles = b) },
        "printlines" to { b, ss -> ss.copy(printLines = b) },
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

    private val stringActionMap: Map<String, ((String, SearchSettings) -> SearchSettings)> = mapOf(
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
        "linesaftertopattern" to
                { s, ss -> ss.copy(linesAfterToPatterns = ss.linesAfterToPatterns.plus(Regex(s))) },
        "linesafteruntilpattern" to
                { s, ss -> ss.copy(linesAfterUntilPatterns = ss.linesAfterUntilPatterns.plus(Regex(s))) },
        "maxlastmod" to
                { s, ss -> ss.copy(maxLastMod = getLastModFromString(s)) },
        "minlastmod" to
                { s, ss -> ss.copy(minLastMod = getLastModFromString(s)) },
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
        "path" to
                { s, ss -> ss.copy(paths = addPath(s, ss.paths)) },
        "searchpattern" to
                { s, ss -> ss.copy(searchPatterns = ss.searchPatterns.plus(Regex(s))) },
        "sort-by" to
                { s, ss -> ss.copy(sortBy = SortBy.forName(s)) },
    )

    private val intActionMap: Map<String, ((Int, SearchSettings) -> SearchSettings)> = mapOf(
        "linesafter" to { i, ss -> ss.copy(linesAfter = i) },
        "linesbefore" to { i, ss -> ss.copy(linesBefore = i) },
        "maxdepth" to { i, ss -> ss.copy(maxDepth = i) },
        "maxlinelength" to { i, ss -> ss.copy(maxLineLength = i) },
        "mindepth" to { i, ss -> ss.copy(minDepth = i) },
    )

    private val longActionMap: Map<String, ((Long, SearchSettings) -> SearchSettings)> = mapOf(
        "maxsize" to { l, ss -> ss.copy(maxSize = l) },
        "minsize" to { l, ss -> ss.copy(minSize = l) },
    )

    private fun applySettings(key: String, lst: List<Any>, settings: SearchSettings): SearchSettings {
        return if (lst.isEmpty()) settings
        else {
            applySettings(key, lst.drop(1), applySetting(key, lst.first(), settings))
        }
    }

    private fun applySetting(key: String, obj: Any, settings: SearchSettings): SearchSettings {
        if (boolActionMap.containsKey(key)) {
            if (obj is Boolean) {
                return this.boolActionMap[key]!!.invoke(obj, settings)
            } else {
                throw SearchException("Invalid value for option: $key")
            }
        } else if (stringActionMap.containsKey(key)) {
            return if (obj is String) {
                this.stringActionMap[key]!!.invoke(obj, settings)
            } else if (obj is JSONArray) {
                applySettings(key, obj.toList(), settings)
            } else {
                throw SearchException("Invalid value for option: $key")
            }
        } else if (intActionMap.containsKey(key)) {
            return if (obj is Int) {
                this.intActionMap[key]!!.invoke(obj, settings)
            } else if (obj is Long) {
                this.intActionMap[key]!!.invoke(obj.toInt(), settings)
            } else {
                throw SearchException("Invalid value for option: $key")
            }
        } else if (longActionMap.containsKey(key)) {
            return if (obj is Int) {
                this.longActionMap[key]!!.invoke(obj.toLong(), settings)
            } else if (obj is Long) {
                this.longActionMap[key]!!.invoke(obj, settings)
            } else {
                throw SearchException("Invalid value for option: $key")
            }
        } else {
            throw SearchException("Invalid option: $key")
        }
    }

    fun settingsFromJson(json: String, settings: SearchSettings): SearchSettings {
        val jsonObject = JSONObject(JSONTokener(json))
        fun recSettingsFromJson(keys: List<String>, settings: SearchSettings) : SearchSettings {
            return if (keys.isEmpty()) settings
            else {
                val ko = keys.first()
                val vo = jsonObject.get(ko)
                if (vo != null) {
                    recSettingsFromJson(keys.drop(1), applySetting(ko, vo, settings))
                } else {
                    recSettingsFromJson(keys.drop(1), settings)
                }
            }
        }
        // keys are sorted so that output is consistent across all versions
        val keys = jsonObject.keySet().toList().sorted()
        val invalidKeys = keys.filter { !longArgMap.containsKey(it) }
        if (invalidKeys.isNotEmpty()) {
            throw SearchException("Invalid option: ${invalidKeys[0]}")
        }
        return recSettingsFromJson(keys, settings)
    }

    private fun settingsFromFile(filePath: String, settings: SearchSettings) : SearchSettings {
        val path = FileUtil.expandPath(Paths.get(filePath));
        if (!Files.exists(path)) {
            throw SearchException("Settings file not found: $filePath")
        }
        if (!filePath.endsWith(".json")) {
            throw SearchException("Invalid settings file (must be JSON): $filePath")
        }
        try {
            val json = path.toFile().readText()
            return settingsFromJson(json, settings)
        } catch (_: FileNotFoundException) {
            throw SearchException("Settings file not found: $filePath")
        } catch (_: IOException) {
            throw SearchException("IOException reading settings file: $filePath")
        } catch (_: JSONException) {
            throw SearchException("Unable to parse JSON in settings file: $filePath")
        }
    }

    fun settingsFromArgs(args : Array<String>) : SearchSettings {
        fun recSettingsFromArgs(args: List<String>, settings: SearchSettings): SearchSettings {
            if (args.isEmpty()) return settings
            val nextArg = args.first()
            if (nextArg.startsWith("-")) {
                val arg = nextArg.dropWhile { it == '-' }
                val longArg = longArgMap[arg]
                return if (boolActionMap.containsKey(longArg)) {
                    val ss = boolActionMap[longArg]!!.invoke(true, settings)
                    recSettingsFromArgs(args.drop(1), ss)
                } else if (stringActionMap.containsKey(longArg)
                    || intActionMap.containsKey(longArg)
                    || longActionMap.containsKey(longArg)
                ) {
                    if (args.size > 1) {
                        val argVal = args.drop(1).first()
                        val ss = if (stringActionMap.containsKey(longArg)) {
                            stringActionMap[longArg]!!.invoke(argVal, settings)
                        } else if (intActionMap.containsKey(longArg)) {
                            intActionMap[longArg]!!.invoke(argVal.toInt(), settings)
                        } else if (longActionMap.containsKey(longArg)) {
                            longActionMap[longArg]!!.invoke(argVal.toLong(), settings)
                        } else {
                            throw SearchException("Unhandled option $arg")
                        }
                        recSettingsFromArgs(args.drop(2), ss)
                    } else {
                        throw SearchException("Missing value for option $arg")
                    }
                } else if (longArg == "settings-file") {
                    if (args.size > 1) {
                        recSettingsFromArgs(args.drop(2), settingsFromFile(args[1], settings))
                    } else {
                        throw SearchException("Missing value for option $arg")
                    }
                } else {
                    throw SearchException("Invalid option: $arg")
                }
            } else {
                return recSettingsFromArgs(args.drop(1), settings.copy(paths = addPath(nextArg, settings.paths)))
            }
        }
        // default printResults to true since running as cli
        return recSettingsFromArgs(args.toList(), getDefaultSettings().copy(printResults = true))
    }

    fun usage() {
        log(getUsageString())
    }

    private fun getUsageString(): String {
        val sb = StringBuilder()
        sb.append("Usage:\n")
        sb.append(" ktsearch [options] -s <searchpattern> <path> [<path> ...]\n\n")
        sb.append("Options:\n")
        fun getOptString(so: SearchOption): String {
            return (if (so.shortArg == null) "" else "-${so.shortArg},") + "--${so.longArg}"
        }

        val optPairs = searchOptions.map { Pair(getOptString(it), it.desc) }
        val longest = optPairs.maxOfOrNull { it.first.length }
        val format = " %1${'$'}-${longest}s  %2${'$'}s\n"
        for (o in optPairs) {
            sb.append(String.format(format, o.first, o.second))
        }
        return sb.toString()
    }
}
