package ktsearch

import ktfind.FileType
import java.time.LocalDateTime

import ktfind.FindSettings
import ktfind.SortBy
import ktfind.fileTypeFromName

/**
 * @author cary on 7/23/16.
 */
data class SearchSettings(val archivesOnly: Boolean,
                          val colorize: Boolean,
                          val debug: Boolean,
                          val excludeHidden: Boolean,
                          val firstMatch: Boolean,
                          val inArchiveExtensions: Set<String>,
                          val inArchiveFilePatterns: Set<Regex>,
                          val inDirPatterns: Set<Regex>,
                          val inExtensions: Set<String>,
                          val inFilePatterns: Set<Regex>,
                          val inFileTypes: Set<FileType>,
                          val inLinesAfterPatterns: Set<Regex>,
                          val inLinesBeforePatterns: Set<Regex>,
                          val linesAfter: Int,
                          val linesAfterToPatterns: Set<Regex>,
                          val linesAfterUntilPatterns: Set<Regex>,
                          val linesBefore: Int,
                          val listDirs: Boolean,
                          val listFiles: Boolean,
                          val listLines: Boolean,
                          val maxLastMod: LocalDateTime?,
                          val maxLineLength: Int,
                          val maxSize: Int,
                          val minLastMod: LocalDateTime?,
                          val minSize: Int,
                          val multiLineSearch: Boolean,
                          val outArchiveExtensions: Set<String>,
                          val outArchiveFilePatterns: Set<Regex>,
                          val outDirPatterns: Set<Regex>,
                          val outExtensions: Set<String>,
                          val outFilePatterns: Set<Regex>,
                          val outFileTypes: Set<FileType>,
                          val outLinesAfterPatterns: Set<Regex>,
                          val outLinesBeforePatterns: Set<Regex>,
                          val paths: Set<String>,
                          val printResults: Boolean,
                          val printUsage: Boolean,
                          val printVersion: Boolean,
                          val recursive: Boolean,
                          val searchArchives: Boolean,
                          val searchPatterns: Set<Regex>,
                          val sortBy: SortBy,
                          val sortCaseInsensitive: Boolean,
                          val sortDescending: Boolean,
                          val textFileEncoding: String,
                          val uniqueLines: Boolean,
                          val verbose: Boolean) {
    val findSettings: FindSettings = FindSettings(archivesOnly,
        debug,
        excludeHidden,
        inArchiveExtensions,
        inArchiveFilePatterns,
        inDirPatterns,
        inExtensions,
        inFilePatterns,
        inFileTypes,
        searchArchives,
        listDirs,
        listFiles,
        maxLastMod,
        maxSize,
        minLastMod,
        minSize,
        outArchiveExtensions,
        outArchiveFilePatterns,
        outDirPatterns,
        outExtensions,
        outFilePatterns,
        outFileTypes,
        paths,
        printUsage,
        printVersion,
        recursive,
        sortBy,
        sortCaseInsensitive,
        sortDescending,
        verbose
    )
}

fun getDefaultSettings() : SearchSettings {
    return SearchSettings(
        archivesOnly = false,
        colorize = true,
        debug = false,
        excludeHidden = true,
        firstMatch = false,
        inArchiveExtensions = setOf(),
        inArchiveFilePatterns = setOf(),
        inDirPatterns = setOf(),
        inExtensions = setOf(),
        inFilePatterns = setOf(),
        inFileTypes = setOf(),
        inLinesAfterPatterns = setOf(),
        inLinesBeforePatterns = setOf(),
        linesAfter = 0,
        linesAfterToPatterns = setOf(),
        linesAfterUntilPatterns = setOf(),
        linesBefore = 0,
        listDirs = false,
        listFiles = false,
        listLines = false,
        maxLastMod = null,
        maxLineLength = 150,
        maxSize = 0,
        minLastMod = null,
        minSize = 0,
        multiLineSearch = false,
        outArchiveExtensions = setOf(),
        outArchiveFilePatterns = setOf(),
        outDirPatterns = setOf(),
        outExtensions = setOf(),
        outFilePatterns = setOf(),
        outFileTypes = setOf(),
        outLinesAfterPatterns = setOf(),
        outLinesBeforePatterns = setOf(),
        paths = setOf(),
        printResults = false,
        printUsage = false,
        printVersion = false,
        recursive = true,
        searchArchives = false,
        searchPatterns = setOf(),
        sortBy = SortBy.FILEPATH,
        sortCaseInsensitive = false,
        sortDescending = false,
        textFileEncoding = "UTF-8",
        uniqueLines = false,
        verbose = false)
}

fun addExtensions(ext: String, extensions: Set<String>): Set<String> {
    val exts = ext.split(',').filter { it.isNotEmpty() }
    return extensions.plus(exts)
}

fun addFileTypes(ft: String, filetypes: Set<FileType>): Set<FileType> {
    val fts = ft.split(',').filter { it.isNotEmpty() }.map { fileTypeFromName(it) }
    return filetypes.plus(fts)
}

fun setArchivesOnly(ss: SearchSettings, archivesOnly: Boolean): SearchSettings {
    return ss.copy(archivesOnly = archivesOnly, searchArchives = archivesOnly || ss.searchArchives)
}

fun setDebug(ss: SearchSettings, debug: Boolean): SearchSettings {
    return ss.copy(debug = debug, verbose = debug || ss.verbose)
}
