package scalasearch

import scala.collection.mutable
import scala.util.matching.Regex

object DefaultSettings {
  val outDirPatterns = Set("""^\.git$""".r, """^\.svn$""".r, """^CVS$""".r)
  val outFilePatterns = Set("""^\.DS_Store$""".r)
  val maxLineLength = 150
}

class SettingsBuilder {
  var startPath = ""
  val inExtensions = mutable.Set[String]()
  val outExtensions = mutable.Set[String]()
  val inDirPatterns = mutable.Set[Regex]()
  val outDirPatterns = mutable.Set[Regex]() ++ DefaultSettings.outDirPatterns
  val inFilePatterns = mutable.Set[Regex]()
  val outFilePatterns = mutable.Set[Regex]() ++ DefaultSettings.outFilePatterns
  val inArchiveFilePatterns = mutable.Set[Regex]()
  val outArchiveFilePatterns = mutable.Set[Regex]()
  val inLinesBeforePatterns = mutable.Set[Regex]()
  val outLinesBeforePatterns = mutable.Set[Regex]()
  val inLinesAfterPatterns = mutable.Set[Regex]()
  val outLinesAfterPatterns = mutable.Set[Regex]()
  val linesAfterToPatterns = mutable.Set[Regex]()
  val linesAfterUntilPatterns = mutable.Set[Regex]()
  val searchPatterns = mutable.Set[Regex]()

  var archivesOnly = false
  var debug = false
  var doTiming = false
  var excludeHidden = true
  var firstMatch = false
  var linesAfter = 0
  var linesBefore = 0
  var listDirs = false
  var listFiles = false
  var listLines = false
  var maxLineLength = DefaultSettings.maxLineLength
  var multiLineSearch = false
  var printResults = true
  var printUsage = false
  var printVersion = false
  var recursive = true
  var searchArchives = false
  var uniqueLines = false
  var verbose = false

  // could be comma-separated
  def addInExtensions(x:String) {
    x.split(",").foreach(inExtensions.add)
  }

  // could be comma-separated
  def addOutExtensions(x:String) {
    x.split(",").foreach(outExtensions.add)
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
      doTiming,
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

class SearchSettings(val startpath:String,
                     val inExtensions:Set[String],
                     val outExtensions:Set[String],
                     val inDirPatterns:Set[Regex],
                     val outDirPatterns:Set[Regex],
                     val inFilePatterns:Set[Regex],
                     val outFilePatterns:Set[Regex],
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
                     val doTiming:Boolean,
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
    linesBefore > 0 || inLinesBeforePatterns.size > 0 || outLinesBeforePatterns.size > 0
  }

  def hasLinesAfter: Boolean = {
    linesAfter > 0 || inLinesAfterPatterns.size > 0 || outLinesAfterPatterns.size > 0
  }

  override def toString = {
    "SearchSettings(" +
    "startpath: \"" + startpath + "\""  +
    ", inExtensions: " + inExtensions +
    ", outExtensions: " + outExtensions +
    ", inDirPatterns: " + inDirPatterns +
    ", outDirPatterns: " + outDirPatterns +
    ", inFilePatterns: " + inFilePatterns +
    ", outFilePatterns: " + outFilePatterns +
    ", inArchiveFilePatterns: " + inArchiveFilePatterns +
    ", outArchiveFilePatterns: " + outArchiveFilePatterns +
    ", searchPatterns: " + searchPatterns +
    ", archivesOnly: " + archivesOnly +
    ", debug: " + debug +
    ", doTiming: " + doTiming +
    ", excludeHidden: " + excludeHidden +
    ", firstMatch: " + firstMatch +
    ", linesAfter: " + linesAfter +
    ", linesBefore: " + linesBefore +
    ", listDirs: " + listDirs +
    ", listFiles: " + listFiles +
    ", listLines: " + listLines +
    ", maxLineLength: " + maxLineLength +
    ", multiLineSearch: " + multiLineSearch +
    ", printResults: " + printResults +
    ", printUsage: " + printUsage +
    ", printVersion: " + printVersion +
    ", recursive: " + recursive +
    ", searchArchives: " + searchArchives +
    ", uniqueLines: " + uniqueLines +
    ", verbose: " + verbose +
    ")"
  }
}
