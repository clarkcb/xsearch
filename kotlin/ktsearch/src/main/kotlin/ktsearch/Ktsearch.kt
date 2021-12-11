package ktsearch

import java.util.Comparator

fun printUsage(searchOptions: SearchOptions) {
    log("")
    searchOptions.usage()
}

fun printErrorWithUsage(err: String, searchOptions: SearchOptions) {
    log("")
    logError(err + "\n")
    searchOptions.usage()
}

private fun signum(num: Int): Int {
    if (num > 0) {
        return 1
    }
    if (num < 0) {
        return -1
    }
    return 0
}

class SearchResultComparator : Comparator<SearchResult> {
    override fun compare(r1: SearchResult, r2: SearchResult): Int {
        val pathCmp = (r1.file!!.file.parent ?: "").lowercase().
                compareTo((r2.file!!.file.parent ?: "").lowercase())
        if (pathCmp == 0) {
            val fileCmp = r1.file.file.name.lowercase().
                    compareTo(r2.file.file.name.lowercase())
            if (fileCmp == 0) {
                val lineNumCmp = signum(r1.lineNum - r2.lineNum)
                if (lineNumCmp == 0) {
                    return signum(r1.matchStartIndex- r2.matchStartIndex)
                }
                return lineNumCmp
            }
            return fileCmp
        }
        return pathCmp
    }
}

fun printResults(results: List<SearchResult>, settings: SearchSettings) {
    log("\nSearch results (${results.size}):")
    val formatter = SearchResultFormatter(settings)
    for (r in results.sortedWith(SearchResultComparator())) {
        log(formatter.format(r))
    }
}

fun printMatchingDirs(results: List<SearchResult>) {
    val dirs = results.mapNotNull { r -> r.file }.mapNotNull { f -> f.file.parent }.distinct().sorted()
    log("\nDirectories with matches (${dirs.size}):")
    for (d in dirs) {
        log(d)
    }
}

fun printMatchingFiles(results: List<SearchResult>) {
    val files = results.mapNotNull { r -> r.file }.map { f -> f.toString() }.distinct().sorted()
    log("\nFiles with matches (${files.size}):")
    for (f in files) {
        log(f)
    }
}

fun printMatchingLines(settings: SearchSettings, results: List<SearchResult>) {
    val lines: List<String> =
            if (settings.uniqueLines) results.map { r -> r.line.trim() }.
                    distinct().sorted()
            else results.map { r -> r.line.trim() }.sorted()
    val hdr =
            if (settings.uniqueLines) "\nUnique lines with matches (${lines.size}):"
            else "\nLines with matches (${lines.size}):"
    log(hdr)
    for (l in lines) {
        log(l)
    }
}

fun search(settings: SearchSettings) {
    val searcher = Searcher(settings)
    val results: List<SearchResult> = searcher.search()

    if (settings.printResults) {
        printResults(results, settings)
    }
    if (settings.listDirs) {
        printMatchingDirs(results)
    }
    if (settings.listFiles) {
        printMatchingFiles(results)
    }
    if (settings.listLines) {
        printMatchingLines(settings, results)
    }
}

fun main(args : Array<String>) {
    val searchOptions = SearchOptions()
    try {
        val settings = searchOptions.settingsFromArgs(args)
        if (settings.debug) log("settings: $settings")
        if (settings.printUsage) printUsage(searchOptions)
        else search(settings)
    } catch (e: SearchException) {
        printErrorWithUsage(e.message ?: "Unknown error", searchOptions)
    }
}
