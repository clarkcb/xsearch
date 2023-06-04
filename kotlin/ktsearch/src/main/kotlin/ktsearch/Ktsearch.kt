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

fun getSearchResultComparator(searcher: Searcher): Comparator<SearchResult> {
    return Comparator { sr1: SearchResult, sr2: SearchResult -> searcher.compareResults(sr1, sr2) }
}

fun printResults(results: List<SearchResult>, searcher: Searcher) {
    log("\nSearch results (${results.size}):")
    val formatter = SearchResultFormatter(searcher.settings)
    for (r in results.sortedWith(getSearchResultComparator(searcher))) {
        log(formatter.format(r))
    }
}

fun printMatchingDirs(results: List<SearchResult>) {
    val dirs = results.asSequence().mapNotNull { r -> r.file }
        .map { f -> f.path.parent }
        .map { p -> p?.toString() ?: "." }
        .distinct().sorted().toList()
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
        printResults(results, searcher)
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
