package scalasearch

import scalafind.{FindSettings, SortBy}
import scalafind.FileType.FileType
import scalafind.SortBy.SortBy

import java.nio.file.Path
import java.time.LocalDateTime
import scala.util.matching.Regex

object DefaultSearchSettings {
  val archivesOnly = false
  val colorize = true
  val debug = false
  val firstMatch = false
  val followSymlinks = false
  val includeHidden = false
  val linesAfter = 0
  val linesBefore = 0
  val maxDepth: Int = -1
  val maxLineLength = 150
  val maxSize: Long = 0L
  val minDepth: Int = -1
  val minSize: Long = 0L
  var multiLineSearch = false
  var paths: Set[Path] = Set.empty[Path]
  val printDirs = false
  val printFiles = false
  val printLines = false
  var printResults = false
  var printUsage = false
  var printVersion = false
  var recursive = true
  var searchArchives = false
  var textFileEncoding: String = "UTF-8"
  var uniqueLines = false
  var verbose = false
}

case class SearchSettings(archivesOnly: Boolean = DefaultSearchSettings.archivesOnly,
                          colorize: Boolean = DefaultSearchSettings.colorize,
                          debug: Boolean = DefaultSearchSettings.debug,
                          firstMatch: Boolean = DefaultSearchSettings.firstMatch,
                          followSymlinks: Boolean = DefaultSearchSettings.followSymlinks,
                          inArchiveExtensions: Set[String] = Set.empty[String],
                          inArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                          includeHidden: Boolean = DefaultSearchSettings.includeHidden,
                          inDirPatterns: Set[Regex] = Set.empty[Regex],
                          inExtensions: Set[String] = Set.empty[String],
                          inFilePatterns: Set[Regex] = Set.empty[Regex],
                          inFileTypes: Set[FileType] = Set.empty[FileType],
                          inLinesAfterPatterns: Set[Regex] = Set.empty[Regex],
                          inLinesBeforePatterns: Set[Regex] = Set.empty[Regex],
                          linesAfter: Int = DefaultSearchSettings.linesAfter,
                          linesAfterToPatterns: Set[Regex] = Set.empty[Regex],
                          linesAfterUntilPatterns: Set[Regex] = Set.empty[Regex],
                          linesBefore: Int = DefaultSearchSettings.linesBefore,
                          maxDepth: Int = DefaultSearchSettings.maxDepth,
                          maxLastMod: Option[LocalDateTime] = None,
                          maxLineLength: Int = DefaultSearchSettings.maxLineLength,
                          maxSize: Long = DefaultSearchSettings.maxSize,
                          minDepth: Int = DefaultSearchSettings.minDepth,
                          minLastMod: Option[LocalDateTime] = None,
                          minSize: Long = DefaultSearchSettings.minSize,
                          multiLineSearch: Boolean = DefaultSearchSettings.multiLineSearch,
                          outArchiveExtensions: Set[String] = Set.empty[String],
                          outArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                          outDirPatterns: Set[Regex] = Set.empty[Regex],
                          outExtensions: Set[String] = Set.empty[String],
                          outFilePatterns: Set[Regex] = Set.empty[Regex],
                          outFileTypes: Set[FileType] = Set.empty[FileType],
                          outLinesAfterPatterns: Set[Regex] = Set.empty[Regex],
                          outLinesBeforePatterns: Set[Regex] = Set.empty[Regex],
                          paths: Set[Path] = DefaultSearchSettings.paths,
                          printDirs: Boolean = DefaultSearchSettings.printDirs,
                          printFiles: Boolean = DefaultSearchSettings.printFiles,
                          printLines: Boolean = DefaultSearchSettings.printLines,
                          printResults: Boolean = DefaultSearchSettings.printResults,
                          printUsage: Boolean = DefaultSearchSettings.printUsage,
                          printVersion: Boolean = DefaultSearchSettings.printVersion,
                          recursive: Boolean = DefaultSearchSettings.recursive,
                          var searchArchives: Boolean = DefaultSearchSettings.searchArchives,
                          searchPatterns: Set[Regex] = Set.empty[Regex],
                          sortBy: SortBy = SortBy.FilePath,
                          textFileEncoding: String = DefaultSearchSettings.textFileEncoding,
                          uniqueLines: Boolean = DefaultSearchSettings.uniqueLines,
                          sortCaseInsensitive: Boolean = false,
                          sortDescending: Boolean = false,
                          var verbose: Boolean = DefaultSearchSettings.verbose) {

  searchArchives = archivesOnly || searchArchives
  verbose = debug || verbose

  val findSettings: FindSettings = FindSettings(
    archivesOnly = archivesOnly,
    debug = debug,
    followSymlinks = followSymlinks,
    inArchiveExtensions = inArchiveExtensions,
    inArchiveFilePatterns = inArchiveFilePatterns,
    includeHidden = includeHidden,
    inDirPatterns = inDirPatterns,
    inExtensions = inExtensions,
    inFilePatterns = inFilePatterns,
    inFileTypes = inFileTypes,
    includeArchives = searchArchives,
    maxDepth = maxDepth,
    maxLastMod = maxLastMod,
    maxSize = maxSize,
    minDepth = minDepth,
    minLastMod = minLastMod,
    minSize = minSize,
    outArchiveExtensions = outArchiveExtensions,
    outArchiveFilePatterns = outArchiveFilePatterns,
    outDirPatterns = outDirPatterns,
    outExtensions = outExtensions,
    outFilePatterns = outFilePatterns,
    outFileTypes = outFileTypes,
    paths = paths,
    printDirs = printDirs,
    printFiles = printFiles,
    printUsage = printUsage,
    printVersion = printVersion,
    recursive = recursive,
    sortBy = sortBy,
    sortCaseInsensitive = sortCaseInsensitive,
    sortDescending = sortDescending,
    verbose = verbose)

  def addExtensions(exts: String, extensions: Set[String]): Set[String] = {
    findSettings.addExtensions(exts, extensions)
  }

  def getLastModFromString(lastModString: String): Option[LocalDateTime] = {
    findSettings.getLastModFromString(lastModString)
  }

  def hasLinesBefore: Boolean = {
    linesBefore > 0 || hasLinesBeforePatterns
  }

  def hasLinesBeforePatterns: Boolean = {
    inLinesBeforePatterns.nonEmpty || outLinesBeforePatterns.nonEmpty
  }

  def hasLinesAfter: Boolean = {
    linesAfter > 0 || hasLinesAfterPatterns
  }

  def hasLinesAfterPatterns: Boolean = {
    inLinesAfterPatterns.nonEmpty || outLinesAfterPatterns.nonEmpty
  }

  def hasLinesAfterToPatterns: Boolean = linesAfterToPatterns.nonEmpty

  def hasLinesAfterUntilPatterns: Boolean = linesAfterUntilPatterns.nonEmpty

  def hasLinesAfterToOrUntilPatterns: Boolean = {
    hasLinesAfterToPatterns || hasLinesAfterUntilPatterns
  }

  override def toString: String = {
    "SearchSettings(" +
      "archivesOnly: " + archivesOnly +
      ", colorize: " + colorize +
      ", debug: " + debug +
      ", firstMatch: " + firstMatch +
      ", followSymlinks: " + followSymlinks +
      ", inArchiveExtensions: " + inArchiveExtensions +
      ", inArchiveFilePatterns: " + inArchiveFilePatterns +
      ", includeHidden: " + includeHidden +
      ", inDirPatterns: " + inDirPatterns +
      ", inExtensions: " + inExtensions +
      ", inFilePatterns: " + inFilePatterns +
      ", inFileTypes: " + inFileTypes +
      ", linesAfter: " + linesAfter +
      ", linesBefore: " + linesBefore +
      ", maxDepth: " + maxDepth +
      ", maxLastMod: " + maxLastMod +
      ", maxLineLength: " + maxLineLength +
      ", maxSize: " + maxSize +
      ", minDepth: " + minDepth +
      ", minLastMod: " + minLastMod +
      ", minSize: " + minSize +
      ", multiLineSearch: " + multiLineSearch +
      ", outArchiveExtensions: " + outArchiveExtensions +
      ", outArchiveFilePatterns: " + outArchiveFilePatterns +
      ", outDirPatterns: " + outDirPatterns +
      ", outExtensions: " + outExtensions +
      ", outFilePatterns: " + outFilePatterns +
      ", outFileTypes: " + outFileTypes +
      ", paths: " + paths  +
      ", printDirs: " + printDirs +
      ", printFiles: " + printFiles +
      ", printLines: " + printLines +
      ", printResults: " + printResults +
      ", printUsage: " + printUsage +
      ", printVersion: " + printVersion +
      ", recursive: " + recursive +
      ", searchArchives: " + searchArchives +
      ", searchPatterns: " + searchPatterns +
      ", sortBy: " + sortBy +
      ", sortCaseInsensitive: " + sortCaseInsensitive +
      ", sortDescending: " + sortDescending +
      ", textFileEncoding: " + textFileEncoding +
      ", uniqueLines: " + uniqueLines +
      ", verbose: " + verbose +
      ")"
  }
}
