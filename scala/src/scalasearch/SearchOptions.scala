package scalasearch

object SearchOptions {

    type OptionMap = Map[Symbol, Any]

    def usage(status: Int) = {
        val usage = """
    Usage:
    search [-x ext[,ext]] [-f '<regex>'] -s '<searchstring>' <startpath>
            """
        println(usage)
        sys.exit(status)
    }

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

    class ArgSearchOption(val shortarg: String, val longarg: String, val func: Function2[String, SearchSettings, Unit], val desc: String) {
        override def toString() = {
            "SearchOption(shortarg: " + shortarg + ", longarg: " + longarg + ")"
        }
    }

    class FlagSearchOption(val shortarg: String, val longarg: String, val func: Function1[SearchSettings, Unit], val desc: String) {
        override def toString() = {
            "SearchOption(shortarg: " + shortarg + ", longarg: " + longarg + ")"
        }
    }

    val ARG_OPTIONS = List(
        new ArgSearchOption("b", "numlinesbefore",
            (x: String, settings: SearchSettings) => { println("numlinesbefore: #{x}") },
            "Number of lines to show before every matched line (default: 0)"),
        new ArgSearchOption("B", "numlinesafter",
            (x: String, settings: SearchSettings) => { println("numlinesafter: #{x}") },
            "Number of lines to show after every matched line (default: 0)"),
        new ArgSearchOption("d", "dirpattern",
            //(x: String, settings: SearchSettings) => { settings.in_dirpatterns.push(Regexp.new(x)) },
            (x: String, settings: SearchSettings) => { println("dirpattern: ") + x },
            "Specify name pattern for directories to include in search"),
        new ArgSearchOption("D", "dirfilter",
            //(x: String, settings: SearchSettings) => { settings.out_dirpatterns.push(Regexp.new(x)) },
            (x: String, settings: SearchSettings) => { println("dirfilter: ") + x },
            "Specify name pattern for directories to exclude from search"),
        new ArgSearchOption("f", "filepattern",
            //(x: String, settings: SearchSettings) => { settings.in_filepatterns.push(Regexp.new(x)) },
            (x: String, settings: SearchSettings) => { println("filepattern: ") + x },
            "Specify name pattern for files to include in search"),
        new ArgSearchOption("F", "filefilter",
            //(x: String, settings: SearchSettings) => { settings.out_filepatterns.push(Regexp.new(x)) },
            (x: String, settings: SearchSettings) => { println("filefilter: ") + x },
            "Specify name pattern for files to exclude from search"),
        new ArgSearchOption("", "linesafterfilter",
            (x: String, settings: SearchSettings) => { println("linesafterfilter: #{x}") },
            "Specify pattern to filter the \"lines-after\" lines on (used with --numlinesafter)"),
        new ArgSearchOption("", "linesaftersearch",
            (x: String, settings: SearchSettings) => { println("linesaftersearch: #{x}") },
            "Specify pattern to search the \"lines-after\" lines on (used with --numlinesafter)"),
        new ArgSearchOption("", "linesbeforefilter",
            (x: String, settings: SearchSettings) => { println("linesbeforefilter: #{x}") },
            "Specify pattern to filter the \"lines-before\" lines on (used with --numlinesbefore)"),
        new ArgSearchOption("", "linesbeforesearch",
            (x: String, settings: SearchSettings) => { println("linesbeforesearch: #{x}") },
            "Specify pattern to search the \"lines-before\" lines on (used with --numlinesbefore)"),
        new ArgSearchOption("s", "search",
            //(x: String, settings: SearchSettings) => { settings.searchpatterns.push(Regexp.new(x)) },
            (x: String, settings: SearchSettings) => { println("searchpattern: ") + x },
            "Specify search pattern"),
        new ArgSearchOption("x", "ext",
            //(x: String, settings: SearchSettings) => { settings.in_extensions.push(x) },
            (x: String, settings: SearchSettings) => { println("ext: ") + x },
            "Specify extension for files to include in search"),
        new ArgSearchOption("X", "extfilter",
            //(x: String, settings: SearchSettings) => { settings.out_extensions.push(x) },
            (x: String, settings: SearchSettings) => { println("extfilter: ") + x },
            "Specify extension for files to exclude from search")
    )

    val FLAG_OPTIONS  = List(
        new FlagSearchOption("1", "firstmatch",
            (settings: SearchSettings) => { settings.firstmatch = true },
            "Capture only the first match for a file+search combination"),
        new FlagSearchOption("a", "allmatches",
            (settings: SearchSettings) => { settings.firstmatch = false },
            "Capture all matches*"),
        new FlagSearchOption("", "debug",
            (settings: SearchSettings) => { settings.debug = true },
            "Set output mode to debug"),
        new FlagSearchOption("h", "help",
            (settings: SearchSettings) => { settings.printusage = true },
            "Print this usage and exit"),
        new FlagSearchOption("", "listfiles",
            (settings: SearchSettings) => { settings.listfiles = true },
            "Generate a list of the matching files after searching"),
        new FlagSearchOption("", "listlines",
            (settings: SearchSettings) => { settings.listlines = true },
            "Generate a list of the matching lines after searching"),
        new FlagSearchOption("m", "multilinesearch",
            (settings: SearchSettings) => { settings.multilinesearch = true },
            "Search files by line*"),
        new FlagSearchOption("p", "printmatches",
            (settings: SearchSettings) => { settings.printresults = true },
            "Print matches to stdout as found*"),
        new FlagSearchOption("P", "noprintmatches",
            (settings: SearchSettings) => { settings.printresults = false },
            "Suppress printing of matches to stdout"),
        new FlagSearchOption("t", "dotiming",
            (settings: SearchSettings) => { settings.dotiming = true },
            "Time search execution"),
        new FlagSearchOption("v", "verbose",
            (settings: SearchSettings) => { settings.verbose = true },
            "Specify verbose output"),
        new FlagSearchOption("V", "version",
            (settings: SearchSettings) => { settings.printversion = true },
            "Print the version and exit"),
        new FlagSearchOption("z", "searchcompressed",
            (settings: SearchSettings) => { settings.searchcompressed = true },
            "Search compressed files (bz2, gz, tar, zip)*"),
        new FlagSearchOption("Z", "nosearchcompressed",
            (settings: SearchSettings) => { settings.searchcompressed = false },
            "Do not search compressed files (bz2, gz, tar, zip)")
    )

    def main(args: Array[String]) = {
        println("Hello from SearchOptions")
        ARG_OPTIONS.foreach(s => println(s.toString))
        FLAG_OPTIONS.foreach(s => println(s.toString))
    }

    def getSearchSettingsFromArgs(args: List[String]): SearchSettings = {
        val settings = new SearchSettings()
        settings
    }

    /*def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
        //def isSwitch(s : String) = (s(0) == '-')
        def setFromString(s: String) = { s.split(",").toSet }
        def getNewSet(set: Any, elem: String) = {
            set match {
                case s: Set[String]  => setFromString(elem) ++ s
                case _               => setFromString(elem)
            }
        }
        def addSetElementToMap(map: OptionMap, sym: Symbol, elem: String) = {
            map ++ Map(sym -> getNewSet(map.getOrElse(sym, Set()), elem))
        }
        list match {
            case Nil => map
            case "-d" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 'd, value), tail) //'
            case "-D" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 'D, value), tail) //'
            case "-f" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 'f, value), tail) //'
            case "-F" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 'F, value), tail) //'
            case "-h" :: Nil =>   usage(0)
            case "-s" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 's, value), tail) //'
            case "-x" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 'x, value), tail) //'
            case "-X" :: value :: tail =>
                                  nextOption(addSetElementToMap(map, 'X, value), tail) //'
            case opt1 :: Nil =>   nextOption(map ++ Map('startpath -> opt1), list.tail) //'
            case opt1 :: tail =>  println("Unknown option " + opt1)
                                  usage(1)
        }
    }*/
    //val options = nextOption(Map(), arglist)
}
