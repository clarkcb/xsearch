package scalasearch

import scala.collection.mutable
import scala.util.matching.Regex

object DefaultSettings {
  val outDirPatterns = Set("""^\.git$""".r, """^\.svn$""".r, """^CVS$""".r)
  val outFilePatterns = Set("""^\.DS_Store$""".r)
}

class SettingsBuilder {
  val inExtensions = mutable.Set[String]()
  val outExtensions = mutable.Set[String]()
  val inDirPatterns = mutable.Set[Regex]()
  val outDirPatterns = mutable.Set[Regex]() ++ DefaultSettings.outDirPatterns
  val inFilePatterns = mutable.Set[Regex]()
  val outFilePatterns = mutable.Set[Regex]() ++ DefaultSettings.outFilePatterns
  val searchPatterns = mutable.Set[Regex]()
  val inLinesBeforePatterns = mutable.Set[Regex]()
  val outLinesBeforePatterns = mutable.Set[Regex]()
  val inLinesAfterPatterns = mutable.Set[Regex]()
  val outLinesAfterPatterns = mutable.Set[Regex]()

  var casesensitive = true
  var debug = false
  var dotiming = false
  var firstmatch = false
  var listdirs = false
  var listfiles = false
  var listlines = false
  var multilinesearch = false
  var numlinesafter = 0
  var numlinesbefore = 0
  var printresults = true
  var printusage = false
  var printversion = false
  var searchcompressed = false
  var startpath = ""
  var verbose = false

  // could be comma-separated
  def addInExtensions(x:String) {
    x.split(",").foreach(inExtensions.add)
  }

  // could be comma-separated
  def addOutExtensions(x:String) {
    x.split(",").foreach(outExtensions.add)
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

  def addInLinesAfterPattern(p:String) {
    inLinesAfterPatterns.add(p.r)
  }

  def addOutLinesAfterPattern(p:String) {
    outLinesAfterPatterns.add(p.r)
  }

  def addInLinesBeforePattern(p:String) {
    inLinesBeforePatterns.add(p.r)
  }

  def addOutLinesBeforePattern(p:String) {
    outLinesBeforePatterns.add(p.r)
  }

  def addSearchPattern(p:String) {
    searchPatterns.add(p.r)
  }

  def toSettings:SearchSettings = {
    new SearchSettings(
      Set.empty[String] ++ inExtensions,
      Set.empty[String] ++ outExtensions,
      Set.empty[Regex] ++ inDirPatterns,
      Set.empty[Regex] ++ outDirPatterns,
      Set.empty[Regex] ++ inFilePatterns,
      Set.empty[Regex] ++ outFilePatterns,
      Set.empty[Regex] ++ searchPatterns,
      Set.empty[Regex] ++ inLinesBeforePatterns,
      Set.empty[Regex] ++ outLinesBeforePatterns,
      Set.empty[Regex] ++ inLinesAfterPatterns,
      Set.empty[Regex] ++ outLinesAfterPatterns,
      casesensitive,
      debug,
      dotiming,
      firstmatch,
      listdirs,
      listfiles,
      listlines,
      multilinesearch,
      numlinesafter,
      numlinesbefore,
      printresults,
      printusage,
      printversion,
      searchcompressed,
      startpath,
      verbose)
  }
}

object SearchSettings {
  // creates SearchSettings with default values
  def apply():SearchSettings = {
    (new SettingsBuilder).toSettings
  }
}

class SearchSettings(val inExtensions:Set[String],
                     val outExtensions:Set[String],
                     val inDirPatterns:Set[Regex],
                     val outDirPatterns:Set[Regex],
                     val inFilePatterns:Set[Regex],
                     val outFilePatterns:Set[Regex],
                     val searchPatterns:Set[Regex],
                     val inLinesBeforePatterns:Set[Regex],
                     val outLinesBeforePatterns:Set[Regex],
                     val inLinesAfterPatterns:Set[Regex],
                     val outLinesAfterPatterns:Set[Regex],
                     val casesensitive:Boolean,
                     val debug:Boolean,
                     val dotiming:Boolean,
                     val firstmatch:Boolean,
                     val listdirs:Boolean,
                     val listfiles:Boolean,
                     val listlines:Boolean,
                     val multilinesearch:Boolean,
                     val numlinesafter:Int,
                     val numlinesbefore:Int,
                     val printresults:Boolean,
                     val printusage:Boolean,
                     val printversion:Boolean,
                     val searchcompressed:Boolean,
                     val startpath:String,
                     val verbose:Boolean) {

  override def toString = {
    "SearchSettings(" +
    "startpath: \"" + startpath + "\""  +
    ", inExtensions: " + inExtensions +
    ", outExtensions: " + outExtensions +
    ", inDirPatterns: " + inDirPatterns +
    ", outDirPatterns: " + outDirPatterns +
    ", inFilePatterns: " + inFilePatterns +
    ", outFilePatterns: " + outFilePatterns +
    ", searchPatterns: " + searchPatterns +
    ")"
  }
}
