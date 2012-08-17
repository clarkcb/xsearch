package scalasearch

import java.util._

class SearchSettings() {
  val inExtensions = new HashSet[String]()
  val outExtensions = new HashSet[String]()
  val inDirPatterns = new HashSet[String]()
  val outDirPatterns = new HashSet[String]()
  val inFilePatterns = new HashSet[String]()
  val outFilePatterns = new HashSet[String]()
  val searchStrings = new HashSet[String]()

  var casesensitive = true
  var debug = false
  var dotiming = false
  var firstmatch = false
  var listfiles = false
  var listlines = false
  var multilinesearch = false
  var printresults = true
  var printusage = false
  var printversion = false
  var searchcompressed = false
  var startpath = ""
  var verbose = false

  override def toString() = {
    "SearchSettings(" +
    //"startpath: " + startpath +
    //"inExtensions: " + inExtensions + ", " +
    //"outExtensions: " + outExtensions + ", " +
    //"inDirPatterns: " + inDirPatterns + ", " +
    //"outDirPatterns: " + outDirPatterns + ", " +
    //"inFilePatterns: " + inFilePatterns + ", " +
    //"outFilePatterns: " + outFilePatterns + ", " +
    //"searchStrings: " + searchStrings + ", " +
    ")"
  }
}
