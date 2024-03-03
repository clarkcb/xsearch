package ktsearch

import ktfind.log
import ktfind.logError

fun printUsage(searchOptions: SearchOptions) {
    log("")
    searchOptions.usage()
}

fun printErrorWithUsage(err: String, searchOptions: SearchOptions) {
    log("")
    logError(err + "\n")
    searchOptions.usage()
}

fun printResults(results: List<SearchResult>, settings: SearchSettings) {
    log("\nSearch results (${results.size}):")
    val formatter = SearchResultFormatter(settings)
    for (r in results) {
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
        printResults(results, settings)
    }
    if (settings.printDirs) {
        printMatchingDirs(results)
    }
    if (settings.printFiles) {
        printMatchingFiles(results)
    }
    if (settings.printLines) {
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
