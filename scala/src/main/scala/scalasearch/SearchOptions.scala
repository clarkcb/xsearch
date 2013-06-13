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
      ((x: String, sb: SettingsBuilder) => sb.inDirPatterns.add(x.r)),
    "in-ext" ->
      ((x: String, sb: SettingsBuilder) => sb.inExtensions.add(x)),
    "in-filepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.inFilePatterns.add(x.r)),
    "in-linesafterpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.inLinesAfterPatterns.add(x.r)),
    "in-linesbeforepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.inLinesBeforePatterns.add(x.r)),
    "linesafter" ->
      ((x: String, sb: SettingsBuilder) => sb.numlinesafter = x.toInt),
    "linesbefore" ->
      ((x: String, sb: SettingsBuilder) => sb.numlinesbefore = x.toInt),
    "out-dirpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.outDirPatterns.add(x.r)),
    "out-ext" ->
      ((x: String, sb: SettingsBuilder) => sb.outExtensions.add(x)),
    "out-filepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.outFilePatterns.add(x.r)),
    "out-linesafterpattern" ->
      ((x: String, sb: SettingsBuilder) => sb.outLinesAfterPatterns.add(x.r)),
    "out-linesbeforepattern" ->
      ((x: String, sb: SettingsBuilder) => sb.outLinesBeforePatterns.add(x.r)),
    "search" ->
      ((x: String, sb: SettingsBuilder) => sb.searchPatterns.add(x.r))
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
    if (!sb.printusage) {
      assert(sb.startpath != "", "Missing startpath")
      assert(sb.searchPatterns.size > 0, "No search patterns defined")
    }
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
