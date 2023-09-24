package scalasearch

import org.json.simple.parser.{JSONParser, ParseException}
import org.json.simple.{JSONArray, JSONObject, JSONValue}
import scalafind.{Common, FileTypes, FileUtil, SortBy}

import java.io.{File, IOException, InputStreamReader}
import java.time.{LocalDateTime, LocalDate}
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class SearchOption(shortarg: Option[String], longarg: String, desc: String) {
  val sortarg: String = shortarg match {
    case Some(sa) => sa.toLowerCase + "@" + longarg.toLowerCase
    case None => longarg.toLowerCase
  }
}

object SearchOptions {
  private val _searchOptionsJsonPath = "/searchoptions.json"
  private val _searchOptions = mutable.ListBuffer.empty[SearchOption]

  private def searchOptions: List[SearchOption] = {
    if (_searchOptions.isEmpty) {
      loadSearchOptionsFromJson()
    }
    List.empty[SearchOption] ++ _searchOptions.sortWith(_.sortarg < _.sortarg)
  }

  private def loadSearchOptionsFromJson(): Unit = {
    try {
      val searchOptionsInputStream = getClass.getResourceAsStream(_searchOptionsJsonPath)
      val obj = new JSONParser().parse(new InputStreamReader(searchOptionsInputStream))
      val jsonObj = obj.asInstanceOf[JSONObject]
      val soIt = jsonObj.get("searchoptions").asInstanceOf[JSONArray].iterator()
      while (soIt.hasNext) {
        val soObj = soIt.next().asInstanceOf[JSONObject]
        val longArg = soObj.get("long").asInstanceOf[String]
        val shortArg =
          if (soObj.containsKey("short")) {
            Some(soObj.get("short").asInstanceOf[String])
          } else {
            None
          }
        val desc = soObj.get("desc").asInstanceOf[String]
        val option = SearchOption(shortArg, longArg, desc)
        _searchOptions += option
      }
    } catch {
      case e: ParseException =>
        print(e.getMessage)
      case e: IOException =>
        print(e.getMessage)
    }
  }

  private def addExtensions(exts: String, extensions: Set[String]): Set[String] = {
    extensions ++ exts.split(",").filterNot(_.isEmpty)
  }

  private def getLastModFromString(lastModString: String): Option[LocalDateTime] = {
    try {
      Some(LocalDateTime.parse(lastModString))
    } catch {
      _ =>
        try {
          val maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE)
          Some(maxLastModDate.atTime(0, 0, 0))
        } catch {
          _ => None
        }
    }
  }

  type ArgAction = (String, SearchSettings) => SearchSettings

  private val argActionMap = Map[String, ArgAction](
    "encoding" ->
      ((s, ss) => ss.copy(textFileEncoding = s)),
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
    "maxdepth" ->
      ((s, ss) => ss.copy(maxDepth = s.toInt)),
    "maxlastmod" ->
      ((s, ss) => ss.copy(maxLastMod = getLastModFromString(s))),
    "maxlinelength" ->
      ((s, ss) => ss.copy(maxLineLength = s.toInt)),
    "maxsize" ->
      ((s, ss) => ss.copy(maxSize = s.toInt)),
    "mindepth" ->
      ((s, ss) => ss.copy(minDepth = s.toInt)),
    "minlastmod" ->
      ((s, ss) => ss.copy(minLastMod = getLastModFromString(s))),
    "minsize" ->
      ((s, ss) => ss.copy(minSize = s.toInt)),
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
    "path" ->
      ((s, ss) => ss.copy(paths = ss.paths + s)),
    "searchpattern" ->
      ((s, ss) => ss.copy(searchPatterns = ss.searchPatterns + s.r)),
    "settings-file" ->
      ((s, ss) => settingsFromFile(s, ss)),
    "sort-by" ->
      ((s, ss) => ss.copy(sortBy = SortBy.fromName(s))),
  )

  type FlagAction = (Boolean, SearchSettings) => SearchSettings

  private val boolFlagActionMap = Map[String, FlagAction](
    "archivesonly" -> ((b, ss) =>
      if (b) ss.copy(archivesOnly = b, searchArchives = b) else ss.copy(archivesOnly = b)),
    "allmatches" -> ((b, ss) => ss.copy(firstMatch = !b)),
    "colorize" -> ((b, ss) => ss.copy(colorize = b)),
    "debug" -> ((b, ss) => if (b) ss.copy(debug = b, verbose = b) else ss.copy(debug = b)),
    "excludehidden" -> ((b, ss) => ss.copy(excludeHidden = b)),
    "firstmatch" -> ((b, ss) => ss.copy(firstMatch = b)),
    "help" -> ((b, ss) => ss.copy(printUsage = b)),
    "includehidden" -> ((b, ss) => ss.copy(excludeHidden = !b)),
    "listdirs" -> ((b, ss) => ss.copy(listDirs = b)),
    "listfiles" -> ((b, ss) => ss.copy(listFiles = b)),
    "listlines" -> ((b, ss) => ss.copy(listLines = b)),
    "multilinesearch" -> ((b, ss) => ss.copy(multiLineSearch = b)),
    "nocolorize" -> ((b, ss) => ss.copy(colorize = !b)),
    "noprintmatches" -> ((b, ss) => ss.copy(printResults = !b)),
    "norecursive" -> ((b, ss) => ss.copy(recursive = !b)),
    "nosearcharchives" -> ((b, ss) => ss.copy(searchArchives = !b)),
    "printmatches" -> ((b, ss) => ss.copy(printResults = b)),
    "recursive" -> ((b, ss) => ss.copy(recursive = b)),
    "searcharchives" -> ((b, ss) => ss.copy(searchArchives = b)),
    "sort-ascending" -> ((b, ss) => ss.copy(sortDescending = !b)),
    "sort-caseinsensitive" -> ((b, ss) => ss.copy(sortCaseInsensitive = b)),
    "sort-casesensitive" -> ((b, ss) => ss.copy(sortCaseInsensitive = !b)),
    "sort-descending" -> ((b, ss) => ss.copy(sortDescending = b)),
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
    recSettingsFromJson(jsonObject.keySet().asScala.map(_.toString).toList, ss)
  }

  @tailrec
  private def applySetting(arg: String, obj: Any, ss: SearchSettings): SearchSettings = obj match {
    case s: String =>
      if (this.argActionMap.contains(arg)) {
        argActionMap(arg)(s, ss)
      } else if (arg == "path") {
        ss.copy(paths = ss.paths + arg)
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
  private def applySettings(arg: String, lst: List[String], ss: SearchSettings): SearchSettings = lst match {
    case Nil => ss
    case h :: t => applySettings(arg, t, applySetting(arg, h, ss))
  }

  private def getArgMap: Map[String, String] = {
    val longOpts: Map[String, String] = searchOptions.map { o => (o.longarg, o.longarg)}.toMap
    val shortOpts = searchOptions.filter(_.shortarg.nonEmpty).map {o => (o.shortarg.get, o.longarg)}.toMap
    longOpts ++ shortOpts
  }

  def settingsFromArgs(args: Array[String]): SearchSettings = {
    val argMap = getArgMap
    val switchPattern = """^-+(\w[\w\-]*)$""".r
    @tailrec
    def nextArg(arglist: List[String], ss: SearchSettings): SearchSettings = {
      arglist match {
        case Nil => ss
        case switchPattern(arg) :: tail =>
          argMap.get(arg) match {
            case Some(longArg) =>
              if (argActionMap.contains(longArg)) {
                if (tail.nonEmpty) {
                  nextArg(tail.tail, argActionMap(longArg)(tail.head, ss))
                } else {
                  throw new SearchException("Missing value for arg %s".format(arg))
                }
              } else if (boolFlagActionMap.contains(longArg)) {
                if (Set("help", "version").contains(longArg)) {
                  nextArg(Nil, boolFlagActionMap(longArg)(true, ss))
                } else {
                  nextArg(tail, boolFlagActionMap(longArg)(true, ss))
                }
              } else {
                throw new SearchException("Invalid option: %s".format(arg))
              }
            case None =>
              throw new SearchException("Invalid option: %s".format(arg))
          }
        case arg :: tail =>
          nextArg(tail, ss.copy(paths = ss.paths + arg))
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
    sb.append(" scalasearch [options] -s <searchpattern> <path> [<path> ...]\n\n")
    sb.append("Options:\n")
    val optPairs = searchOptions.map { so =>
      val opts = so.shortarg match {
        case Some(sa) => s"-$sa,--${so.longarg}"
        case None => s"--${so.longarg}"
      }
      (opts, so.desc)
    }
    val longest = optPairs.map(_._1.length).max
    val format = " %1$-" + longest + "s  %2$s\n"
    optPairs.foreach {op =>
      sb.append(format.format(op._1, op._2))
    }
    sb.toString()
  }
}
