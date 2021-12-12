package scalasearch

import scalasearch.FileType.FileType

import scala.util.matching.Regex

object DefaultSettings {
  val archivesOnly = false
  val colorize = true
  val debug = false
  val excludeHidden = true
  val firstMatch = false
  val linesAfter = 0
  val linesBefore = 0
  val listDirs = false
  val listFiles = false
  val listLines = false
  val maxLineLength = 150
  var multiLineSearch = false
  var paths: Set[String] = Set.empty[String]
  var printResults = false
  var printUsage = false
  var printVersion = false
  var recursive = true
  var searchArchives = false
  var textFileEncoding: String = "UTF-8"
  var uniqueLines = false
  var verbose = false
}

case class SearchSettings(archivesOnly: Boolean = DefaultSettings.archivesOnly,
                          colorize: Boolean = DefaultSettings.colorize,
                          debug: Boolean = DefaultSettings.debug,
                          excludeHidden: Boolean = DefaultSettings.excludeHidden,
                          firstMatch: Boolean = DefaultSettings.firstMatch,
                          inArchiveExtensions: Set[String] = Set.empty[String],
                          inArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                          inDirPatterns: Set[Regex] = Set.empty[Regex],
                          inExtensions: Set[String] = Set.empty[String],
                          inFilePatterns: Set[Regex] = Set.empty[Regex],
                          inFileTypes: Set[FileType] = Set.empty[FileType],
                          inLinesAfterPatterns: Set[Regex] = Set.empty[Regex],
                          inLinesBeforePatterns: Set[Regex] = Set.empty[Regex],
                          linesAfter: Int = DefaultSettings.linesAfter,
                          linesAfterToPatterns: Set[Regex] = Set.empty[Regex],
                          linesAfterUntilPatterns: Set[Regex] = Set.empty[Regex],
                          linesBefore: Int = DefaultSettings.linesBefore,
                          listDirs: Boolean = DefaultSettings.listDirs,
                          listFiles: Boolean = DefaultSettings.listFiles,
                          listLines: Boolean = DefaultSettings.listLines,
                          maxLineLength: Int = DefaultSettings.maxLineLength,
                          multiLineSearch: Boolean = DefaultSettings.multiLineSearch,
                          outArchiveExtensions: Set[String] = Set.empty[String],
                          outArchiveFilePatterns: Set[Regex] = Set.empty[Regex],
                          outDirPatterns: Set[Regex] = Set.empty[Regex],
                          outExtensions: Set[String] = Set.empty[String],
                          outFilePatterns: Set[Regex] = Set.empty[Regex],
                          outFileTypes: Set[FileType] = Set.empty[FileType],
                          outLinesAfterPatterns: Set[Regex] = Set.empty[Regex],
                          outLinesBeforePatterns: Set[Regex] = Set.empty[Regex],
                          paths: Set[String] = DefaultSettings.paths,
                          printResults: Boolean = DefaultSettings.printResults,
                          printUsage: Boolean = DefaultSettings.printUsage,
                          printVersion: Boolean = DefaultSettings.printVersion,
                          recursive: Boolean = DefaultSettings.recursive,
                          var searchArchives: Boolean = DefaultSettings.searchArchives,
                          searchPatterns: Set[Regex] = Set.empty[Regex],
                          textFileEncoding: String = DefaultSettings.textFileEncoding,
                          uniqueLines: Boolean = DefaultSettings.uniqueLines,
                          var verbose: Boolean = DefaultSettings.verbose) {

  searchArchives = archivesOnly || searchArchives
  verbose = debug || verbose

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
      ", excludeHidden: " + excludeHidden +
      ", firstMatch: " + firstMatch +
      ", inArchiveExtensions: " + inArchiveExtensions +
      ", inArchiveFilePatterns: " + inArchiveFilePatterns +
      ", inDirPatterns: " + inDirPatterns +
      ", inExtensions: " + inExtensions +
      ", inFilePatterns: " + inFilePatterns +
      ", inFileTypes: " + inFileTypes +
      ", linesAfter: " + linesAfter +
      ", linesBefore: " + linesBefore +
      ", listDirs: " + listDirs +
      ", listFiles: " + listFiles +
      ", listLines: " + listLines +
      ", maxLineLength: " + maxLineLength +
      ", multiLineSearch: " + multiLineSearch +
      ", outArchiveExtensions: " + outArchiveExtensions +
      ", outArchiveFilePatterns: " + outArchiveFilePatterns +
      ", outDirPatterns: " + outDirPatterns +
      ", outExtensions: " + outExtensions +
      ", outFilePatterns: " + outFilePatterns +
      ", outFileTypes: " + outFileTypes +
      ", paths: " + paths  +
      ", printResults: " + printResults +
      ", printUsage: " + printUsage +
      ", printVersion: " + printVersion +
      ", recursive: " + recursive +
      ", searchArchives: " + searchArchives +
      ", searchPatterns: " + searchPatterns +
      ", textFileEncoding: " + textFileEncoding  +
      ", uniqueLines: " + uniqueLines +
      ", verbose: " + verbose +
      ")"
  }
}
