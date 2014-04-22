package scalasearch

import scala.collection.mutable
import scala.xml._

case class SearchOption(shortarg:String, longarg:String, desc:String) {
  val sortarg = 
    if (shortarg.nonEmpty)
      shortarg.toLowerCase + "a" + longarg.toLowerCase
    else
      longarg.toLowerCase
}

object SearchOptions {
  // TODO: move to config file
  private val _searchOptionsXmlPath = "/searchoptions.xml"
  private val _searchOptions = mutable.ListBuffer.empty[SearchOption]

  private def searchOptions: List[SearchOption] = {
    if (_searchOptions.isEmpty) {
      val root = XML.load(getClass.getResourceAsStream(_searchOptionsXmlPath))
      val searchOptionNodes = root \\ "searchoption"
      for (searchOptionNode <- searchOptionNodes) {
        val short = (searchOptionNode \ "@short").text
        val long = (searchOptionNode \ "@long").text
        val desc = searchOptionNode.text.trim
        val option = SearchOption(short, long, desc)
        _searchOptions += option
      }
    }
    List.empty[SearchOption] ++ _searchOptions.sortWith(_.sortarg < _.sortarg)
  }

  val argActionMap = Map[String, ((String, SettingsBuilder) => Unit)](
    "in-dirpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInDirPattern(x)),
    "in-ext" ->
      ((x: String, sb: SettingsBuilder) => sb.addInExtensions(x)),
    "in-filepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInFilePattern(x)),
    "in-linesafterpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInLinesAfterPattern(x)),
    "in-linesbeforepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addInLinesBeforePattern(x)),
    "linesafter" ->
      ((x: String, sb: SettingsBuilder) => sb.numlinesafter = x.toInt),
    "linesbefore" ->
      ((x: String, sb: SettingsBuilder) => sb.numlinesbefore = x.toInt),
    "out-dirpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutDirPattern(x)),
    "out-ext" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutExtensions(x)),
    "out-filepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutFilePattern(x)),
    "out-linesafterpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutLinesAfterPattern(x)),
    "out-linesbeforepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.addOutLinesBeforePattern(x)),
    "search" ->
      ((x: String, sb: SettingsBuilder) => sb.addSearchPattern(x))
  )

  val flagActionMap = Map[String, (SettingsBuilder => Unit)](
    "allmatches" ->
      ((sb: SettingsBuilder) => sb.firstmatch = false),
    "debug" ->
      ((sb: SettingsBuilder) => sb.debug = true),
    "dotiming" ->
      ((sb: SettingsBuilder) => sb.dotiming = true),
    "firstmatch" ->
      ((sb: SettingsBuilder) => sb.firstmatch = true),
    "help" ->
      ((sb: SettingsBuilder) => sb.printusage = true),
    "listdirs" ->
      ((sb: SettingsBuilder) => sb.listdirs = true),
    "listfiles" ->
      ((sb: SettingsBuilder) => sb.listfiles = true),
    "listlines" ->
      ((sb: SettingsBuilder) => sb.listlines = true),
    "multilinesearch" ->
      ((sb: SettingsBuilder) => sb.multilinesearch = true),
    "noprintmatches" ->
      ((sb: SettingsBuilder) => sb.printresults = false),
    "nosearchcompressed" ->
      ((sb: SettingsBuilder) => sb.searchcompressed = false),
    "printmatches" ->
      ((sb: SettingsBuilder) => sb.printresults = true),
    "searchcompressed" ->
      ((sb: SettingsBuilder) => sb.searchcompressed = true),
    "verbose" ->
      ((sb: SettingsBuilder) => sb.verbose = true),
    "version" ->
      ((sb: SettingsBuilder) => sb.printversion = true)
  )

  def mapFromOptions(options: List[SearchOption]): Map[String,SearchOption] = {
    (options.map(o => (o.longarg, o)) ++
      options.filter(o => o.shortarg.length > 0).map(o => (o.shortarg, o))).toMap
  }

  def settingsFromArgs(args: List[String]): SearchSettings = {
    val sb = new SettingsBuilder
    val argMap = mapFromOptions(searchOptions.filter(o => argActionMap.contains(o.longarg)))
    val flagMap = mapFromOptions(searchOptions.filter(o => flagActionMap.contains(o.longarg)))
    val switchPattern = """^\-+(\w[\w\-]*)$""".r
    def nextArg(arglist:List[String], sb:SettingsBuilder) {
      arglist match {
        case Nil => Unit
        case switchPattern(name) :: tail =>
          if (argMap.contains(name)) {
            if (tail.length > 0) {
              argActionMap(argMap(name).longarg)(tail.head, sb)
              nextArg(tail.tail, sb)
            } else {
              throw new Exception("Arg without required value: "+name)
            }
          } else if (flagMap.contains(name)) {
            flagActionMap(flagMap(name).longarg)(sb)
            nextArg(tail, sb)
          } else {
            throw new Exception("Undefined option: " + name)
          }
        case value :: Nil =>
          sb.startpath = value
        case _ =>
          throw new Exception("Invalid args: "+arglist.mkString(", "))
      }
    }
    nextArg(args, sb)
    if (sb.debug) sb.verbose = true
    sb.toSettings
  }

  def usage(status: Int) = {
    println(getUsageString)
    sys.exit(status)
  }

  def getUsageString = {
    val sb = new StringBuilder
    sb.append("Usage:\n")
    sb.append(" scalasearch [options] <startpath>\n\n")
    sb.append("Options:\n")

    val optStrings =
      (searchOptions.map(o => if (!o.shortarg.isEmpty) "-" + o.shortarg + "," else "")
        zip searchOptions.map("--" + _.longarg)).map(o => o._1 + o._2)
    val optDescs = searchOptions.map(_.desc)
    val longest = optStrings.map(_.length).max
    val format = " %1$-"+longest+"s  %2$s\n"
    for (i <- 0 until optStrings.length) {
      sb.append(format.format(optStrings(i), optDescs(i)))
    }
    sb.toString()
  }
}
