package ktsearch

import kotlin.collections.Set

/**
 * @author cary on 7/23/16.
 */
data class SearchSettings(val archivesOnly: Boolean,
                          val debug: Boolean,
                          val excludeHidden: Boolean,
                          val firstMatch: Boolean,
                          val inArchiveExtensions: Set<String>,
                          val inArchiveFilePatterns: Set<Regex>,
                          val inDirPatterns: Set<Regex>,
                          val inExtensions: Set<String>,
                          val inFilePatterns: Set<Regex>,
                          val inLinesAfterPatterns: Set<Regex>,
                          val inLinesBeforePatterns: Set<Regex>,
                          val linesAfter: Int,
                          val linesAfterToPatterns: Set<Regex>,
                          val linesAfterUntilPatterns: Set<Regex>,
                          val linesBefore: Int,
                          val listDirs: Boolean,
                          val listFiles: Boolean,
                          val listLines: Boolean,
                          val maxLineLength: Int,
                          val multiLineSearch: Boolean,
                          val outArchiveExtensions: Set<String>,
                          val outArchiveFilePatterns: Set<Regex>,
                          val outDirPatterns: Set<Regex>,
                          val outExtensions: Set<String>,
                          val outFilePatterns: Set<Regex>,
                          val outLinesAfterPatterns: Set<Regex>,
                          val outLinesBeforePatterns: Set<Regex>,
                          val printResults: Boolean,
                          val printUsage: Boolean,
                          val printVersion: Boolean,
                          val recursive: Boolean,
                          val searchArchives: Boolean,
                          val searchPatterns: Set<Regex>,
                          val startPath: String?,
                          val uniqueLines: Boolean,
                          val verbose: Boolean) {
}

fun getDefaultSettings() : SearchSettings {
    return SearchSettings(
            archivesOnly = false,
            debug = false,
            excludeHidden = true,
            firstMatch = false,
            inArchiveExtensions = setOf(),
            inArchiveFilePatterns = setOf(),
            inDirPatterns = setOf(),
            inExtensions = setOf(),
            inFilePatterns = setOf(),
            inLinesAfterPatterns = setOf(),
            inLinesBeforePatterns = setOf(),
            linesAfter = 0,
            linesAfterToPatterns = setOf(),
            linesAfterUntilPatterns = setOf(),
            linesBefore = 0,
            listDirs = false,
            listFiles = false,
            listLines = false,
            maxLineLength = 150,
            multiLineSearch = false,
            outArchiveExtensions = setOf(),
            outArchiveFilePatterns = setOf(),
            outDirPatterns = setOf(),
            outExtensions = setOf(),
            outFilePatterns = setOf(),
            outLinesAfterPatterns = setOf(),
            outLinesBeforePatterns = setOf(),
            printResults = false,
            printUsage = false,
            printVersion = false,
            recursive = true,
            searchArchives = false,
            searchPatterns = setOf(),
            startPath = null,
            uniqueLines = false,
            verbose = false)
}

fun addExtensions(ext: String, extensions: Set<String>): Set<String> {
    val exts = ext.split(',').filter { it.isNotEmpty() }
    return extensions.plus(exts)
}
