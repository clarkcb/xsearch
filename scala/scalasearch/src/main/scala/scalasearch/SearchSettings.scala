package scalasearch

import scalafind.Color.Color
import scalafind.FileType.FileType
import scalafind.SortBy.SortBy
import scalafind.{Color, DefaultFindSettings, FindSettings, SortBy}

import java.nio.file.Path
import java.time.LocalDateTime
import scala.util.matching.Regex

object DefaultSearchSettings {
  val archivesOnly: Boolean = DefaultFindSettings.archivesOnly
  val colorize: Boolean = DefaultFindSettings.colorize
  val debug: Boolean = DefaultFindSettings.debug
  val dirColor: Color = DefaultFindSettings.dirColor
  val extColor: Color = DefaultFindSettings.extColor
  val fileColor: Color = DefaultFindSettings.fileColor
  val firstMatch = false
  val followSymlinks: Boolean = DefaultFindSettings.followSymlinks
  val includeHidden: Boolean = DefaultFindSettings.includeHidden
  val lineColor: Color = Color.GREEN
  val linesAfter = 0
  val linesBefore = 0
  val maxDepth: Int = DefaultFindSettings.maxDepth
  val maxSize: Long = DefaultFindSettings.maxSize
  val minDepth: Int = DefaultFindSettings.minDepth
  val minSize: Long = DefaultFindSettings.minSize
  val maxLineLength = 150
  var multiLineSearch = false
  val printDirs: Boolean = DefaultFindSettings.printDirs
  val printFiles: Boolean = DefaultFindSettings.printFiles
  val printLines = false
  val printMatches = false
  var printResults = false
  val printUsage: Boolean = DefaultFindSettings.printUsage
  val printVersion: Boolean = DefaultFindSettings.printVersion
  val recursive: Boolean = DefaultFindSettings.recursive
  var searchArchives = false
  var textFileEncoding: String = "UTF-8"
  var uniqueLines = false
  val verbose: Boolean = DefaultFindSettings.verbose
}

case class SearchSettings(archivesOnly: Boolean = DefaultSearchSettings.archivesOnly,
                          colorize: Boolean = DefaultSearchSettings.colorize,
                          debug: Boolean = DefaultSearchSettings.debug,
                          dirColor: Color = DefaultSearchSettings.dirColor,
                          extColor: Color = DefaultSearchSettings.extColor,
                          fileColor: Color = DefaultSearchSettings.fileColor,
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
                          lineColor: Color = DefaultSearchSettings.lineColor,
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
                          paths: Set[Path] = Set.empty[Path],
                          printDirs: Boolean = DefaultSearchSettings.printDirs,
                          printFiles: Boolean = DefaultSearchSettings.printFiles,
                          printLines: Boolean = DefaultSearchSettings.printLines,
                          printMatches: Boolean = DefaultSearchSettings.printMatches,
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
    colorize = colorize,
    debug = debug,
    dirColor = dirColor,
    extColor = extColor,
    fileColor = fileColor,
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
      ", printMatches: " + printMatches +
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
