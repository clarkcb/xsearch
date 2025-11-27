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
data class SearchOption(override val shortArg: String?, override val longArg: String,
                        override val desc: String, override val argType: ArgTokenType) : Option {
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
    private var argTokenizer: ArgTokenizer

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
        "settings-file" to
                { s, ss -> updateSettingsFromFile(ss, s) },
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

    private fun loadSearchOptionsFromJson() : List<SearchOption> {
        val searchOptionsInputStream = javaClass.getResourceAsStream(searchOptionsJsonPath)
        val jsonObj = JSONObject(JSONTokener(searchOptionsInputStream))
        val searchOptionsArray = jsonObj.getJSONArray("searchoptions").iterator()
        val options : MutableList<SearchOption> = mutableListOf()
        while (searchOptionsArray.hasNext()) {
            val searchOptionObj = searchOptionsArray.next() as JSONObject
            val longArg = searchOptionObj.getString("long")
            val shortArg =
                if (searchOptionObj.has("short")) {
                    val sArg = searchOptionObj.getString("short")
                    sArg
                } else {
                    null
                }
            val desc = searchOptionObj.getString("desc")
            var argType = ArgTokenType.UNKNOWN
            if (boolActionMap.containsKey(longArg)) {
                argType = ArgTokenType.BOOL
            } else if (stringActionMap.containsKey(longArg)) {
                argType = ArgTokenType.STR
            } else if (intActionMap.containsKey(longArg)) {
                argType = ArgTokenType.INT
            } else if (longActionMap.containsKey(longArg)) {
                argType = ArgTokenType.LONG
            }
            options.add(SearchOption(shortArg, longArg, desc, argType))
        }
        return options.toList()
    }

    init {
        searchOptions = loadSearchOptionsFromJson()
        argTokenizer = ArgTokenizer(searchOptions)
    }

    private fun applyArgTokenToSettings(argToken: ArgToken, settings: SearchSettings): SearchSettings {
        if (argToken.type == ArgTokenType.BOOL) {
            if (argToken.value is Boolean) {
                return this.boolActionMap[argToken.name]!!.invoke(argToken.value as Boolean, settings)
            } else {
                throw SearchException("Invalid value for option: ${argToken.name}")
            }
        } else if (argToken.type == ArgTokenType.STR) {
            return when (argToken.value) {
                is String -> {
                    this.stringActionMap[argToken.name]!!.invoke(argToken.value as String, settings)
                }
                is Iterable<*> -> {
                    var updatedSettings: SearchSettings = settings
                    for (v in argToken.value as Iterable<*>) {
                        if (v is String) {
                            updatedSettings = this.stringActionMap[argToken.name]!!.invoke(v, updatedSettings)
                        } else {
                            throw SearchException("Invalid value for option: ${argToken.name}")
                        }
                    }
                    updatedSettings
                }
                else -> {
                    throw SearchException("Invalid value for option: ${argToken.name}")
                }
            }
        } else if (argToken.type == ArgTokenType.INT) {
            return when (argToken.value) {
                is Int -> {
                    this.intActionMap[argToken.name]!!.invoke(argToken.value as Int, settings)
                }
                is Long -> {
                    this.intActionMap[argToken.name]!!.invoke((argToken.value as Long).toInt(), settings)
                }
                else -> {
                    throw SearchException("Invalid value for option: ${argToken.name}")
                }
            }
        } else if (argToken.type == ArgTokenType.LONG) {
            return when (argToken.value) {
                is Int -> {
                    this.longActionMap[argToken.name]!!.invoke((argToken.value as Int).toLong(), settings)
                }
                is Long -> {
                    this.longActionMap[argToken.name]!!.invoke(argToken.value as Long, settings)
                }
                else -> {
                    throw SearchException("Invalid value for option: ${argToken.name}")
                }
            }
        } else {
            throw SearchException("Invalid option: ${argToken.name}")
        }
    }

    private fun updateSettingsFromArgTokens(settings: SearchSettings, argTokens: List<ArgToken>): SearchSettings {
        var updatedSettings: SearchSettings = settings
        for (argToken in argTokens) {
            updatedSettings = applyArgTokenToSettings(argToken, updatedSettings)
        }
        return updatedSettings
    }

    fun updateSettingsFromJson(settings: SearchSettings, json: String): SearchSettings {
        val argTokens = argTokenizer.tokenizeJson(json)
        return updateSettingsFromArgTokens(settings, argTokens)
    }

    fun updateSettingsFromFile(settings: SearchSettings, filePath: String): SearchSettings {
        val argTokens = argTokenizer.tokenizeFile(filePath)
        return updateSettingsFromArgTokens(settings, argTokens)
    }

    fun updateSettingsFromArgs(settings: SearchSettings, args: Array<String>): SearchSettings {
        val argTokens = argTokenizer.tokenizeArgs(args)
        return updateSettingsFromArgTokens(settings, argTokens)
    }

    fun settingsFromArgs(args: Array<String>): SearchSettings {
        if (args.isEmpty()) {
            throw SearchException(FindError.STARTPATH_NOT_DEFINED.message)
        }
        val settings = getDefaultSettings().copy(printResults = true)
        return updateSettingsFromArgs(settings, args)
    }

    private fun getUsageString(): String {
        val sb = StringBuilder()
        sb.append("Usage:\n")
        sb.append(" ktsearch [options] -s <searchpattern> <path> [<path> ...]\n\n")
        sb.append("Options:\n")
        fun getOptString(so: SearchOption): String {
            return (if (so.shortArg == null) "" else "-${so.shortArg},") + "--${so.longArg}"
        }

        val optPairs = searchOptions.sortedBy { it.sortArg }.map { Pair(getOptString(it), it.desc) }
        val longest = optPairs.maxOfOrNull { it.first.length }
        val format = $$" %1$-$${longest}s  %2$s\n"
        for (o in optPairs) {
            sb.append(String.format(format, o.first, o.second))
        }
        return sb.toString()
    }

    fun usage() {
        log(getUsageString())
    }
}
