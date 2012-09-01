package scalasearch

import scala.collection.mutable.Set
import scala.util.matching.Regex

class SearchSettings {
  val inExtensions = Set[String]()
  val outExtensions = Set[String]()
  val inDirPatterns = Set[Regex]()
  val outDirPatterns = Set[Regex]()
  val inFilePatterns = Set[Regex]()
  val outFilePatterns = Set[Regex]()
  val searchPatterns = Set[Regex]()
  val inLinesBeforePatterns = Set[Regex]()
  val outLinesBeforePatterns = Set[Regex]()
  val inLinesAfterPatterns = Set[Regex]()
  val outLinesAfterPatterns = Set[Regex]()

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
