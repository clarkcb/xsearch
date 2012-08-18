package scalasearch

object SearchOptions {

  abstract class SearchOption(val shortarg: String, val longarg: String, val desc: String) {
    def apply(settings:SearchSettings)
    def apply(s:String, settings:SearchSettings)
    def sortArg = {
      if (shortarg.length > 0)
        shortarg
      else
        longarg
    }
    override def toString = {
      "SearchOption(shortarg: " + shortarg + ", longarg: " + longarg + ")"
    }
  }

  class ArgSearchOption(shortarg: String, longarg: String, desc: String,
      val func: (String, SearchSettings) => Unit) extends SearchOption (
      shortarg, longarg, desc) {
    override def apply(settings:SearchSettings) = Unit
    override def apply(s:String, settings:SearchSettings) = func(s, settings)
  }

  class FlagSearchOption(shortarg: String, longarg: String, desc: String,
      val func: (SearchSettings) => Unit) extends SearchOption (
      shortarg, longarg, desc) {
    override def apply(settings:SearchSettings) = func(settings)
    override def apply(s:String, settings:SearchSettings) = Unit
  }

  val argOptions = List(
    //new ArgSearchOption("b", "numlinesbefore",
    //  "Number of lines to show before every matched line (default: 0)",
    //  (x: String, settings: SearchSettings) => { println("numlinesbefore: #{x}") }),
    //new ArgSearchOption("B", "numlinesafter",
    //  "Number of lines to show after every matched line (default: 0)",
    //   (x: String, settings: SearchSettings) => { println("numlinesafter: #{x}") }),
    new ArgSearchOption("d", "dirpattern",
      "Specify name pattern for directories to include in search",
      (x: String, settings: SearchSettings) => settings.inDirPatterns.add(x.r)),
    new ArgSearchOption("D", "dirfilter",
      "Specify name pattern for directories to exclude from search",
      (x: String, settings: SearchSettings) => { settings.outDirPatterns.add(x.r) }),
    new ArgSearchOption("f", "filepattern",
      "Specify name pattern for files to include in search",
      (x: String, settings: SearchSettings) => { settings.inFilePatterns.add(x.r) }),
    new ArgSearchOption("F", "filefilter",
      "Specify name pattern for files to exclude from search",
      (x: String, settings: SearchSettings) => { settings.outFilePatterns.add(x.r) }),
    //new ArgSearchOption("", "linesafterfilter",
    //  "Specify pattern to filter the \"lines-after\" lines on (used with --numlinesafter)",
    //  (x: String, settings: SearchSettings) => { println("linesafterfilter: #{x}") }),
    //new ArgSearchOption("", "linesaftersearch",
    //  "Specify pattern to search the \"lines-after\" lines on (used with --numlinesafter)",
    //  (x: String, settings: SearchSettings) => { println("linesaftersearch: #{x}") }),
    //new ArgSearchOption("", "linesbeforefilter",
    //  "Specify pattern to filter the \"lines-before\" lines on (used with --numlinesbefore)",
    //  (x: String, settings: SearchSettings) => { println("linesbeforefilter: #{x}") }),
    //new ArgSearchOption("", "linesbeforesearch",
    //  "Specify pattern to search the \"lines-before\" lines on (used with --numlinesbefore)",
    //  (x: String, settings: SearchSettings) => { println("linesbeforesearch: #{x}") }),
    new ArgSearchOption("s", "search",
      "Specify search pattern",
      (x: String, settings: SearchSettings) => { settings.searchPatterns.add(x.r) }),
    new ArgSearchOption("x", "ext",
      "Specify extension for files to include in search",
      (x: String, settings: SearchSettings) => { settings.inExtensions.add(x) }),
    new ArgSearchOption("X", "extfilter",
      "Specify extension for files to exclude from search",
      (x: String, settings: SearchSettings) => { settings.outExtensions.add(x) })
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

  def mapFromOptions(options: List[SearchOption]): Map[String,SearchOption] = {
    val optionTuples = (options map (o => (o.longarg, o))) :::
      ((options filter (o => o.shortarg.length > 0)) map (o => (o.shortarg, o)))
    optionTuples.toMap
  }

  def settingsFromArgs(args: List[String]): SearchSettings = {
    val settings = new SearchSettings()
    val argMap = mapFromOptions(argOptions)
    val flagMap = mapFromOptions(flagOptions)
    val switchPattern = """^\-+(\w[\w\-]*)$""".r
    def nextArg(arglist:List[String], settings:SearchSettings): Unit = {
      arglist match {
        case Nil => Unit
        case switchPattern(name) :: tail =>
          if (argMap.contains(name)) {
            if (tail.length > 0) {
              argMap(name)(tail.head, settings)
              nextArg(tail.tail, settings)
            } else {
              throw new Exception("Arg without required value: "+name)
            }
          } else if (flagMap.contains(name)) {
            flagMap(name)(settings)
            nextArg(tail, settings)
          } else {
            throw new Exception("Undefined option: " + name)
          }
        case value :: Nil =>
          settings.startpath = value
        case _ =>
          throw new Exception("Invalid args: "+arglist.mkString(", "))
      }
    }
    nextArg(args, settings)
    if (!settings.printusage) {
      assert(settings.startpath != "", "Missing startpath")
      assert(settings.searchPatterns.size > 0, "No search patterns defined")
    }
    settings
  }

  def usage(status: Int) = {
    println(getUsageString)
    //sys.exit(status)
  }

  def getUsageString = {
    val sb = new StringBuilder()
    sb.append("Usage:\n")
    sb.append(" scalasearch [options] <startpath>\n\n")
    sb.append("Options:\n")

    var options:List[SearchOption] = (argOptions ::: flagOptions).sort(
      (o1, o2) => (o1.sortArg.toLowerCase compareTo o2.sortArg.toLowerCase) < 0)

    val optStrings = ((options.map(o => if (o.shortarg.length > 0) "-" + o.shortarg + "," else "")) zip (options.map(o => "--" + o.longarg))).map(o => o._1 + o._2)
    val optDescs = options.map(o => o.desc)
    val longest = optStrings.map(o => o.length).sort((o1, o2) => o1 > o2).head
    val format = " %1$-"+longest+"s  %2$s\n"
    for (i <- 0 until optStrings.length) {
      sb.append(String.format(format, optStrings(i), optDescs(i)))
    }
    sb.toString
  }
}




