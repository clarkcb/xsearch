//package scalasearch

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

  abstract class SearchOption(val shortarg: String, val longarg: String, val desc: String) {
    override def toString = {
      "SearchOption(shortarg: " + shortarg + ", longarg: " + longarg + ")"
    }
  }

  class ArgSearchOption(shortarg: String, longarg: String, desc: String,
      val func: Function2[String, SearchSettings, Unit]) extends SearchOption (
      shortarg, longarg, desc)

  class FlagSearchOption(shortarg: String, longarg: String, desc: String,
      val func: Function1[SearchSettings, Unit]) extends SearchOption (
      shortarg, longarg, desc)

  val argTuples = List(
    ("b", "numlinesbefore",
     "Number of lines to show before every matched line (default: 0)",
     (x: String, settings: SearchSettings) => { println("numlinesbefore: #{x}") }),
    ("B", "numlinesafter",
     "Number of lines to show after every matched line (default: 0)",
     (x: String, settings: SearchSettings) => { println("numlinesafter: #{x}") })
  )
  val argOptions = List(
    new ArgSearchOption("b", "numlinesbefore",
      "Number of lines to show before every matched line (default: 0)",
      (x: String, settings: SearchSettings) => { println("numlinesbefore: #{x}") }),
    new ArgSearchOption("B", "numlinesafter",
      "Number of lines to show after every matched line (default: 0)",
       (x: String, settings: SearchSettings) => { println("numlinesafter: #{x}") }),
    new ArgSearchOption("d", "dirpattern",
      "Specify name pattern for directories to include in search",
      //(x: String, settings: SearchSettings) => { settings.in_dirpatterns.push(Regexp.new(x)) },
      (x: String, settings: SearchSettings) => { println("dirpattern: ") + x }),
    new ArgSearchOption("D", "dirfilter",
      "Specify name pattern for directories to exclude from search",
      //(x: String, settings: SearchSettings) => { settings.out_dirpatterns.push(Regexp.new(x)) },
      (x: String, settings: SearchSettings) => { println("dirfilter: ") + x }),
    new ArgSearchOption("f", "filepattern",
      "Specify name pattern for files to include in search",
      //(x: String, settings: SearchSettings) => { settings.in_filepatterns.push(Regexp.new(x)) },
      (x: String, settings: SearchSettings) => { println("filepattern: ") + x }),
    new ArgSearchOption("F", "filefilter",
      "Specify name pattern for files to exclude from search",
      //(x: String, settings: SearchSettings) => { settings.out_filepatterns.push(Regexp.new(x)) },
      (x: String, settings: SearchSettings) => { println("filefilter: ") + x }),
    new ArgSearchOption("", "linesafterfilter",
      "Specify pattern to filter the \"lines-after\" lines on (used with --numlinesafter)",
      (x: String, settings: SearchSettings) => { println("linesafterfilter: #{x}") }),
    new ArgSearchOption("", "linesaftersearch",
      "Specify pattern to search the \"lines-after\" lines on (used with --numlinesafter)",
      (x: String, settings: SearchSettings) => { println("linesaftersearch: #{x}") }),
    new ArgSearchOption("", "linesbeforefilter",
      "Specify pattern to filter the \"lines-before\" lines on (used with --numlinesbefore)",
      (x: String, settings: SearchSettings) => { println("linesbeforefilter: #{x}") }),
    new ArgSearchOption("", "linesbeforesearch",
      "Specify pattern to search the \"lines-before\" lines on (used with --numlinesbefore)",
      (x: String, settings: SearchSettings) => { println("linesbeforesearch: #{x}") }),
    new ArgSearchOption("s", "search",
      "Specify search pattern",
      //(x: String, settings: SearchSettings) => { settings.searchpatterns.push(Regexp.new(x)) },
      (x: String, settings: SearchSettings) => { println("searchpattern: ") + x }),
    new ArgSearchOption("x", "ext",
      //(x: String, settings: SearchSettings) => { settings.in_extensions.push(x) },
      "Specify extension for files to include in search",
      (x: String, settings: SearchSettings) => { println("ext: ") + x }),
    new ArgSearchOption("X", "extfilter",
      "Specify extension for files to exclude from search",
      //(x: String, settings: SearchSettings) => { settings.out_extensions.push(x) },
      (x: String, settings: SearchSettings) => { println("extfilter: ") + x })
  )

  val flagOptions  = List(
    new FlagSearchOption("1", "firstmatch",
      "Capture only the first match for a file+search combination",
      (settings: SearchSettings) => { settings.firstmatch = true }),
    new FlagSearchOption("a", "allmatches",
      "Capture all matches*",
      (settings: SearchSettings) => { settings.firstmatch = false }),
    new FlagSearchOption("", "debug",
      "Set output mode to debug",
      (settings: SearchSettings) => { settings.debug = true }),
    new FlagSearchOption("h", "help",
      "Print this usage and exit",
      (settings: SearchSettings) => { settings.printusage = true }),
    new FlagSearchOption("", "listfiles",
      "Generate a list of the matching files after searching",
      (settings: SearchSettings) => { settings.listfiles = true }),
    new FlagSearchOption("", "listlines",
      "Generate a list of the matching lines after searching",
      (settings: SearchSettings) => { settings.listlines = true }),
    new FlagSearchOption("m", "multilinesearch",
      "Search files by line*",
      (settings: SearchSettings) => { settings.multilinesearch = true }),
    new FlagSearchOption("p", "printmatches",
      "Print matches to stdout as found*",
      (settings: SearchSettings) => { settings.printresults = true }),
    new FlagSearchOption("P", "noprintmatches",
      "Suppress printing of matches to stdout",
      (settings: SearchSettings) => { settings.printresults = false }),
    new FlagSearchOption("t", "dotiming",
      "Time search execution",
      (settings: SearchSettings) => { settings.dotiming = true }),
    new FlagSearchOption("v", "verbose",
      "Specify verbose output",
      (settings: SearchSettings) => { settings.verbose = true }),
    new FlagSearchOption("V", "version",
      "Print the version and exit",
      (settings: SearchSettings) => { settings.printversion = true }),
    new FlagSearchOption("z", "searchcompressed",
      "Search compressed files (bz2, gz, tar, zip)*",
      (settings: SearchSettings) => { settings.searchcompressed = true }),
    new FlagSearchOption("Z", "nosearchcompressed",
      "Do not search compressed files (bz2, gz, tar, zip)",
      (settings: SearchSettings) => { settings.searchcompressed = false })
  )

  def getSearchSettingsFromArgs(args: List[String]): SearchSettings = {
    val settings = new SearchSettings()
    val shortargs =
      for (argOption <- argOptions if argOption.shortarg != "") yield (argOption.shortarg, argOption)
    val longargs =
      for (argOption <- argOptions) yield (argOption.longarg, argOption)

    settings
  }

  def main(args: Array[String]) = {
    println("Hello from SearchOptions")
    argOptions.foreach(s => println(s.toString))
    flagOptions.foreach(s => println(s.toString))
  }

  /*def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
    //def isSwitch(s : String) = (s(0) == '-')
    def setFromString(s: String) = { s.split(",").toSet }
    def getNewSet(set: Any, elem: String) = {
      set match {
        case s: Set[String]  => setFromString(elem) ++ s
        case _         => setFromString(elem)
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
