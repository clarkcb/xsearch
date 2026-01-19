package ktsearch

import ktfind.log
import ktfind.logError

fun printUsage(searchOptions: SearchOptions) {
    log("")
    searchOptions.usage()
}

fun printErrorWithUsage(err: String, colorize: Boolean, searchOptions: SearchOptions) {
    log("")
    logError(err + "\n", colorize)
    searchOptions.usage()
}

fun search(settings: SearchSettings) {
    val searcher = Searcher(settings)
    val results: List<SearchResult> = searcher.search()
    val formatter = SearchResultFormatter(settings)

    if (settings.printResults) {
        searcher.printResults(results, formatter)
    }
    if (settings.printDirs) {
        searcher.printMatchingDirs(results, formatter)
    }
    if (settings.printFiles) {
        searcher.printMatchingFiles(results, formatter)
    }
    if (settings.printLines) {
        searcher.printMatchingLines(results, formatter)
    }
    if (settings.printMatches) {
        searcher.printMatches(results, formatter)
    }
}

fun main(args : Array<String>) {
    val searchOptions = SearchOptions()
    var colorize = true
    try {
        val settings = searchOptions.settingsFromArgs(args)
        colorize = settings.colorize
        if (settings.debug) log("settings: $settings")
        if (settings.printUsage) printUsage(searchOptions)
        else search(settings)
    } catch (e: SearchException) {
        printErrorWithUsage(e.message ?: "Unknown error", colorize, searchOptions)
    }
}
