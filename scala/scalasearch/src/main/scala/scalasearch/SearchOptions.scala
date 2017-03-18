package scalasearch

import java.io.File
import java.util
import org.json.simple.{JSONArray, JSONObject, JSONValue}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml.XML
import scala.collection.JavaConversions._

case class SearchOption(shortarg:String, longarg:String, desc:String) {
  val sortarg =
    if (shortarg.nonEmpty) {
      shortarg.toLowerCase + "@" + longarg.toLowerCase
    } else {
      longarg.toLowerCase
    }
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

  private def addExtensions(exts: String, extensions: Set[String]): Set[String] = {
    extensions ++ exts.split(",").filterNot(_.isEmpty)
  }

  type ArgAction = (String, SearchSettings) => SearchSettings

  private val argActionMap = Map[String, ArgAction](
    "in-archiveext" ->
      ((s, ss) => ss.copy(inArchiveExtensions = addExtensions(s, ss.inArchiveExtensions))),
    "in-archivefilepattern" ->
      ((s, ss) => ss.copy(inArchiveFilePatterns = ss.inArchiveFilePatterns + s.r)),
    "in-dirpattern" ->
      ((s, ss) => ss.copy(inDirPatterns = ss.inDirPatterns + s.r)),
    "in-ext" ->
      ((s, ss) => ss.copy(inExtensions = addExtensions(s, ss.inExtensions))),
    "in-filepattern" ->
      ((s, ss) => ss.copy(inFilePatterns = ss.inFilePatterns + s.r)),
    "in-filetype" ->
      ((s, ss) => ss.copy(inFileTypes = ss.inFileTypes + FileTypes.fromName(s))),
    "in-linesafterpattern" ->
      ((s, ss) => ss.copy(inLinesAfterPatterns  = ss.inLinesAfterPatterns + s.r)),
    "in-linesbeforepattern" ->
      ((s, ss) => ss.copy(inLinesBeforePatterns = ss.inLinesBeforePatterns + s.r)),
    "linesafter" ->
      ((s, ss) => ss.copy(linesAfter = s.toInt)),
    "linesaftertopattern" ->
      ((s, ss) => ss.copy(linesAfterToPatterns  = ss.linesAfterToPatterns + s.r)),
    "linesafteruntilpattern" ->
      ((s, ss) => ss.copy(linesAfterUntilPatterns = ss.linesAfterUntilPatterns + s.r)),
    "linesbefore" ->
      ((s, ss) => ss.copy(linesBefore = s.toInt)),
    "maxlinelength" ->
      ((s, ss) => ss.copy(maxLineLength = s.toInt)),
    "out-archiveext" ->
      ((s, ss) => ss.copy(outArchiveExtensions = addExtensions(s, ss.outArchiveExtensions))),
    "out-archivefilepattern" ->
      ((s, ss) => ss.copy(outArchiveFilePatterns = ss.outArchiveFilePatterns + s.r)),
    "out-dirpattern" ->
      ((s, ss) => ss.copy(outDirPatterns = ss.outDirPatterns + s.r)),
    "out-ext" ->
      ((s, ss) => ss.copy(outExtensions = addExtensions(s, ss.outExtensions))),
    "out-filepattern" ->
      ((s, ss) => ss.copy(outFilePatterns = ss.outFilePatterns + s.r)),
    "out-filetype" ->
      ((s, ss) => ss.copy(outFileTypes = ss.outFileTypes + FileTypes.fromName(s))),
    "out-linesafterpattern" ->
      ((s, ss) => ss.copy(outLinesAfterPatterns = ss.outLinesAfterPatterns + s.r)),
    "out-linesbeforepattern" ->
      ((s, ss) => ss.copy(outLinesBeforePatterns = ss.outLinesBeforePatterns + s.r)),
    "search" ->
      ((s, ss) => ss.copy(searchPatterns = ss.searchPatterns + s.r)),
    "settings-file" ->
      ((s, ss) => settingsFromFile(s, ss))
  )

  type FlagAction = (Boolean, SearchSettings) => SearchSettings

  private val boolFlagActionMap = Map[String, FlagAction](
    "archivesonly" -> ((b, ss) =>
      if (b) ss.copy(archivesOnly = b, searchArchives = b) else ss.copy(archivesOnly = b)),
    "allmatches" -> ((b, ss) => ss.copy(firstMatch = !b)),
    "debug" -> ((b, ss) => if (b) ss.copy(debug = b, verbose = b) else ss.copy(debug = b)),
    "excludehidden" -> ((b, ss) => ss.copy(excludeHidden = b)),
    "firstmatch" -> ((b, ss) => ss.copy(firstMatch = b)),
    "help" -> ((b, ss) => ss.copy(printUsage = b)),
    "includehidden" -> ((b, ss) => ss.copy(excludeHidden = !b)),
    "listdirs" -> ((b, ss) => ss.copy(listDirs = b)),
    "listfiles" -> ((b, ss) => ss.copy(listFiles = b)),
    "listlines" -> ((b, ss) => ss.copy(listLines = b)),
    "multilinesearch" -> ((b, ss) => ss.copy(multiLineSearch = b)),
    "noprintmatches" -> ((b, ss) => ss.copy(printResults = !b)),
    "norecursive" -> ((b, ss) => ss.copy(recursive = !b)),
    "nosearcharchives" -> ((b, ss) => ss.copy(searchArchives = !b)),
    "printmatches" -> ((b, ss) => ss.copy(printResults = b)),
    "recursive" -> ((b, ss) => ss.copy(recursive = b)),
    "searcharchives" -> ((b, ss) => ss.copy(searchArchives = b)),
    "uniquelines" -> ((b, ss) => ss.copy(uniqueLines = b)),
    "verbose" -> ((b, ss) => ss.copy(verbose = b)),
    "version" -> ((b, ss) => ss.copy(printVersion = b))
  )

  private def settingsFromFile(filePath: String, ss: SearchSettings): SearchSettings = {
    val file: File = new File(filePath)
    if (!file.exists()) {
      throw new SearchException("Settings file not found: %s".format(filePath))
    }
    val json: String = FileUtil.getFileContents(file)
    settingsFromJson(json, ss)
  }

  def settingsFromJson(json: String, ss: SearchSettings): SearchSettings = {
    val obj: AnyRef = JSONValue.parseWithException(json)
    val jsonObject: JSONObject = obj.asInstanceOf[JSONObject]
    @tailrec
    def recSettingsFromJson(keys: List[String], settings: SearchSettings): SearchSettings = keys match {
      case Nil => settings
      case k :: ks =>
        val v = jsonObject.get(k)
        recSettingsFromJson(ks, applySetting(k, v, settings))
    }
    recSettingsFromJson(jsonObject.keySet().map(_.toString).toList, ss)
  }

  @tailrec
  def applySetting(arg: String, obj: Any, ss: SearchSettings): SearchSettings = obj match {
    case s: String =>
      if (this.argActionMap.contains(arg)) {
        argActionMap(arg)(s, ss)
      } else if (arg == "startpath") {
        ss.copy(startPath = Some(s))
      } else {
        throw new SearchException("Invalid option: " + arg)
      }
    case b: Boolean =>
      if (this.boolFlagActionMap.contains(arg)) {
        boolFlagActionMap(arg)(b, ss)
      } else {
        throw new SearchException("Invalid option: " + arg)
      }
    case l: Long =>
      applySetting(arg, l.toString, ss)
    case a: JSONArray =>
      applySettings(arg, a.toArray.toList.map(_.toString), ss)
    case _ =>
      throw new SearchException("Unsupported data type")
  }

  @tailrec
  def applySettings(arg: String, lst: List[String], ss: SearchSettings): SearchSettings = lst match {
    case Nil => ss
    case h :: t => applySettings(arg, t, applySetting(arg, h, ss))
  }

  private def getArgMap: Map[String, String] = {
    val longOpts = searchOptions.map {o => (o.longarg, o.longarg)}.toMap
    val shortOpts = searchOptions.filter(_.shortarg.nonEmpty).map {o => (o.shortarg, o.longarg)}.toMap
    longOpts ++ shortOpts
  }

  def settingsFromArgs(args: Array[String]): SearchSettings = {
    val argMap = getArgMap
    val switchPattern = """^\-+(\w[\w\-]*)$""".r
    @tailrec
    def nextArg(arglist: List[String], ss: SearchSettings): SearchSettings = {
      arglist match {
        case Nil => ss
        case switchPattern(arg) :: tail =>
          val longArg = argMap.getOrElse(arg, "")
          if (argActionMap.contains(longArg)) {
            if (tail.nonEmpty) {
              nextArg(tail.tail, argActionMap(longArg)(tail.head, ss))
            } else {
              throw new SearchException("Missing value for arg %s".format(arg))
            }
          } else if (boolFlagActionMap.contains(longArg)) {
            nextArg(tail, boolFlagActionMap(longArg)(true, ss))
          } else {
            throw new SearchException("Invalid option: %s".format(arg))
          }
        case arg :: tail =>
          nextArg(tail, ss.copy(startPath = Some(arg)))
      }
    }
    nextArg(args.toList, SearchSettings(printResults = true))
  }

  def usage(status: Int): Unit = {
    Common.log(getUsageString)
    sys.exit(status)
  }

  def getUsageString: String = {
    val sb = new StringBuilder
    sb.append("Usage:\n")
    sb.append(" scalasearch [options] -s <searchpattern> <startpath>\n\n")
    sb.append("Options:\n")
    val optStrings =
      (searchOptions.map(o => if (o.shortarg.nonEmpty) "-" + o.shortarg + "," else "")
        zip searchOptions.map("--" + _.longarg)).map(o => o._1 + o._2)
    val optDescs = searchOptions.map(_.desc)
    val longest = optStrings.map(_.length).max
    val format = " %1$-" + longest + "s  %2$s\n"
    for (i <- optStrings.indices) {
      sb.append(format.format(optStrings(i), optDescs(i)))
    }
    sb.toString()
  }
}
