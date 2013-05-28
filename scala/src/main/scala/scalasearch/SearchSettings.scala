package scalasearch

import scala.collection.mutable
import scala.util.matching.Regex

class SettingsBuilder {
  val inExtensions = mutable.Set[String]()
  val outExtensions = mutable.Set[String]()
  val inDirPatterns = mutable.Set[Regex]()
  val outDirPatterns = mutable.Set[Regex]()
  val inFilePatterns = mutable.Set[Regex]()
  val outFilePatterns = mutable.Set[Regex]()
  val searchPatterns = mutable.Set[Regex]()
  val inLinesBeforePatterns = mutable.Set[Regex]()
  val outLinesBeforePatterns = mutable.Set[Regex]()
  val inLinesAfterPatterns = mutable.Set[Regex]()
  val outLinesAfterPatterns = mutable.Set[Regex]()

  var casesensitive = true
  var debug = false
  var dotiming = false
  var firstmatch = false
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
