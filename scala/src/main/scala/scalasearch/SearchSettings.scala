package scalasearch

import scala.collection.mutable
import scala.util.matching.Regex

class SearchSettings {
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
