package scalasearch

import scala.collection.mutable
import scala.util.matching.Regex

object DefaultSettings {
  val archivesOnly = false
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
  var printResults = true
  var printUsage = false
  var printVersion = false
  var recursive = true
  var searchArchives = false
  var startPath:Option[String] = None
  var uniqueLines = false
  var verbose = false
}

class SettingsBuilder {
  var startPath = DefaultSettings.startPath
  val inExtensions = mutable.Set[String]()
  val outExtensions = mutable.Set[String]()
  val inDirPatterns = mutable.Set[Regex]()
  val outDirPatterns = mutable.Set[Regex]()
  val inFilePatterns = mutable.Set[Regex]()
  val outFilePatterns = mutable.Set[Regex]()
  val inArchiveExtensions = mutable.Set[String]()
  val outArchiveExtensions = mutable.Set[String]()
  val inArchiveFilePatterns = mutable.Set[Regex]()
  val outArchiveFilePatterns = mutable.Set[Regex]()
  val inLinesBeforePatterns = mutable.Set[Regex]()
  val outLinesBeforePatterns = mutable.Set[Regex]()
  val inLinesAfterPatterns = mutable.Set[Regex]()
  val outLinesAfterPatterns = mutable.Set[Regex]()
  val linesAfterToPatterns = mutable.Set[Regex]()
  val linesAfterUntilPatterns = mutable.Set[Regex]()
  val searchPatterns = mutable.Set[Regex]()

  var archivesOnly = DefaultSettings.archivesOnly
  var debug = DefaultSettings.debug
  var excludeHidden = DefaultSettings.excludeHidden
  var firstMatch = DefaultSettings.firstMatch
  var linesAfter = DefaultSettings.linesAfter
  var linesBefore = DefaultSettings.linesBefore
  var listDirs = DefaultSettings.listDirs
  var listFiles = DefaultSettings.listFiles
  var listLines = DefaultSettings.listLines
  var maxLineLength = DefaultSettings.maxLineLength
  var multiLineSearch = DefaultSettings.multiLineSearch
  var printResults = DefaultSettings.printResults
  var printUsage = DefaultSettings.printUsage
  var printVersion = DefaultSettings.printVersion
  var recursive = DefaultSettings.recursive
  var searchArchives = DefaultSettings.searchArchives
  var uniqueLines = DefaultSettings.uniqueLines
  var verbose = DefaultSettings.verbose

  def setArchivesOnly() {
    archivesOnly = true
    searchArchives = true
  }

  def setDebug() {
    debug = true
    verbose = true
  }

  def addCommaSeparatedExtensions(x:String, extensions:mutable.Set[String]) {
    x.split(",").foreach(extensions.add)
  }

  def addInExtensions(x:String) {
    addCommaSeparatedExtensions(x, inExtensions)
  }

  def addOutExtensions(x:String) {
    addCommaSeparatedExtensions(x, outExtensions)
  }

  def addInArchiveExtensions(x:String) {
    addCommaSeparatedExtensions(x, inArchiveExtensions)
  }

  def addOutArchiveExtensions(x:String) {
    addCommaSeparatedExtensions(x, outArchiveExtensions)
  }

  def addInArchiveFilePattern(p: String) {
    inArchiveFilePatterns.add(p.r)
  }

  def addOutArchiveFilePattern(p: String) {
    outArchiveFilePatterns.add(p.r)
  }

  def addInDirPattern(p:String) {
    inDirPatterns.add(p.r)
  }

  def addOutDirPattern(p:String) {
    outDirPatterns.add(p.r)
  }

  def addInFilePattern(p:String) {
    inFilePatterns.add(p.r)
  }

  def addOutFilePattern(p:String) {
    outFilePatterns.add(p.r)
  }

  def addInLinesBeforePattern(p:String) {
    inLinesBeforePatterns.add(p.r)
  }

  def addOutLinesBeforePattern(p:String) {
    outLinesBeforePatterns.add(p.r)
  }

  def addInLinesAfterPattern(p:String) {
    inLinesAfterPatterns.add(p.r)
  }

  def addOutLinesAfterPattern(p:String) {
    outLinesAfterPatterns.add(p.r)
  }

  def addLinesAfterToPattern(p:String) {
    linesAfterToPatterns.add(p.r)
  }

  def addLinesAfterUntilPattern(p:String) {
    linesAfterUntilPatterns.add(p.r)
  }

  def addSearchPattern(p:String) {
    searchPatterns.add(p.r)
  }

  def toSettings:SearchSettings = {
    new SearchSettings(
      startPath,
      Set.empty[String] ++ inExtensions,
      Set.empty[String] ++ outExtensions,
      Set.empty[Regex] ++ inDirPatterns,
      Set.empty[Regex] ++ outDirPatterns,
      Set.empty[Regex] ++ inFilePatterns,
      Set.empty[Regex] ++ outFilePatterns,
      Set.empty[String] ++ inArchiveExtensions,
      Set.empty[String] ++ outArchiveExtensions,
      Set.empty[Regex] ++ inArchiveFilePatterns,
      Set.empty[Regex] ++ outArchiveFilePatterns,
      Set.empty[Regex] ++ inLinesBeforePatterns,
      Set.empty[Regex] ++ outLinesBeforePatterns,
      Set.empty[Regex] ++ inLinesAfterPatterns,
      Set.empty[Regex] ++ outLinesAfterPatterns,
      Set.empty[Regex] ++ linesAfterToPatterns,
      Set.empty[Regex] ++ linesAfterUntilPatterns,
      Set.empty[Regex] ++ searchPatterns,
      archivesOnly,
      debug,
      excludeHidden,
      firstMatch,
      linesAfter,
      linesBefore,
      listDirs,
      listFiles,
      listLines,
      maxLineLength,
      multiLineSearch,
      printResults,
      printUsage,
      printVersion,
      recursive,
      searchArchives,
      uniqueLines,
      verbose)
  }
}

object SearchSettings {
  // creates SearchSettings with default values
  def apply():SearchSettings = {
    (new SettingsBuilder).toSettings
  }
}

class SearchSettings(val startPath:Option[String],
                     val inExtensions:Set[String],
                     val outExtensions:Set[String],
                     val inDirPatterns:Set[Regex],
                     val outDirPatterns:Set[Regex],
                     val inFilePatterns:Set[Regex],
                     val outFilePatterns:Set[Regex],
                     val inArchiveExtensions:Set[String],
                     val outArchiveExtensions:Set[String],
                     val inArchiveFilePatterns:Set[Regex],
                     val outArchiveFilePatterns:Set[Regex],
                     val inLinesBeforePatterns:Set[Regex],
                     val outLinesBeforePatterns:Set[Regex],
                     val inLinesAfterPatterns:Set[Regex],
                     val outLinesAfterPatterns:Set[Regex],
                     val linesAfterToPatterns:Set[Regex],
                     val linesAfterUntilPatterns:Set[Regex],
                     val searchPatterns:Set[Regex],
                     val archivesOnly:Boolean,
                     val debug:Boolean,
                     val excludeHidden:Boolean,
                     val firstMatch:Boolean,
                     val linesAfter:Int,
                     val linesBefore:Int,
                     val listDirs:Boolean,
                     val listFiles:Boolean,
                     val listLines:Boolean,
                     val maxLineLength:Int,
                     val multiLineSearch:Boolean,
                     val printResults:Boolean,
                     val printUsage:Boolean,
                     val printVersion:Boolean,
                     val recursive:Boolean,
                     val searchArchives:Boolean,
                     val uniqueLines:Boolean,
                     val verbose:Boolean) {

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
      ", debug: " + debug +
      ", excludeHidden: " + excludeHidden +
      ", firstMatch: " + firstMatch +
      ", inArchiveExtensions: " + inArchiveExtensions +
      ", inArchiveFilePatterns: " + inArchiveFilePatterns +
      ", inDirPatterns: " + inDirPatterns +
      ", inExtensions: " + inExtensions +
      ", inFilePatterns: " + inFilePatterns +
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
      ", printResults: " + printResults +
      ", printUsage: " + printUsage +
      ", printVersion: " + printVersion +
      ", recursive: " + recursive +
      ", searchArchives: " + searchArchives +
      ", searchPatterns: " + searchPatterns +
      ", startpath: " + startPath  +
      ", uniqueLines: " + uniqueLines +
      ", verbose: " + verbose +
      ")"
  }
}
