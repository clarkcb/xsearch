class SearchSettings() {
    //val inExtensions = Set[String]
    //val outExtensions = Set[String]
    //val inDirPatterns = Set[String]
    //val outDirPatterns = Set[String]
    //val inFilePatterns = Set[String]
    //val outFilePatterns = Set[String]
    //val searchStrings = Set[String]
    //val startpath = String

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
