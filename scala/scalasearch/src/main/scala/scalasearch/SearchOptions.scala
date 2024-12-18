package scalasearch

import org.json.{JSONArray, JSONObject, JSONTokener}
import scalafind.{Common, FileType, FileUtil, SortBy}

import java.io.{File, IOException, InputStreamReader}
import java.nio.file.Paths
import java.time.LocalDateTime
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

      val jsonObj = new JSONObject(new JSONTokener(new InputStreamReader(searchOptionsInputStream)))
      val searchOptionsArray = jsonObj.getJSONArray("searchoptions").iterator()
      while (searchOptionsArray.hasNext) {
        val searchOptionObj = searchOptionsArray.next().asInstanceOf[JSONObject]
        val longArg = searchOptionObj.getString("long")
        val shortArg =
          if (searchOptionObj.has("short")) {
            Some(searchOptionObj.getString("short"))
          } else {
            None
          }
        val desc = searchOptionObj.getString("desc")
        val option = SearchOption(shortArg, longArg, desc)
        _searchOptions += option
      }
    } catch {
      case e: IOException =>
        print(e.getMessage)
    }
  }

  private type BoolAction = (Boolean, SearchSettings) => SearchSettings

  private val boolActionMap = Map[String, BoolAction](
    "archivesonly" -> ((b, ss) =>
      if (b) ss.copy(archivesOnly = b, searchArchives = b) else ss.copy(archivesOnly = b)),
    "allmatches" -> ((b, ss) => ss.copy(firstMatch = !b)),
    "colorize" -> ((b, ss) => ss.copy(colorize = b)),
    "debug" -> ((b, ss) => if (b) ss.copy(debug = b, verbose = b) else ss.copy(debug = b)),
    "excludehidden" -> ((b, ss) => ss.copy(includeHidden = !b)),
    "firstmatch" -> ((b, ss) => ss.copy(firstMatch = b)),
    "followsymlinks" -> ((b, ss) => ss.copy(followSymlinks = b)),
    "help" -> ((b, ss) => ss.copy(printUsage = b)),
    "includehidden" -> ((b, ss) => ss.copy(includeHidden = b)),
    "multilinesearch" -> ((b, ss) => ss.copy(multiLineSearch = b)),
    "nocolorize" -> ((b, ss) => ss.copy(colorize = !b)),
    "noprintdirs" -> ((b, ss) => ss.copy(printDirs = !b)),
    "noprintfiles" -> ((b, ss) => ss.copy(printFiles = !b)),
    "noprintlines" -> ((b, ss) => ss.copy(printLines = !b)),
    "noprintmatches" -> ((b, ss) => ss.copy(printResults = !b)),
    "norecursive" -> ((b, ss) => ss.copy(recursive = !b)),
    "nosearcharchives" -> ((b, ss) => ss.copy(searchArchives = !b)),
    "printdirs" -> ((b, ss) => ss.copy(printDirs = b)),
    "printfiles" -> ((b, ss) => ss.copy(printFiles = b)),
    "printlines" -> ((b, ss) => ss.copy(printLines = b)),
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

  private type StringAction = (String, SearchSettings) => SearchSettings

  private val stringActionMap = Map[String, StringAction](
    "encoding" ->
      ((s, ss) => ss.copy(textFileEncoding = s)),
    "in-archiveext" ->
      ((s, ss) => ss.copy(inArchiveExtensions = ss.addExtensions(s, ss.inArchiveExtensions))),
    "in-archivefilepattern" ->
      ((s, ss) => ss.copy(inArchiveFilePatterns = ss.inArchiveFilePatterns + s.r)),
    "in-dirpattern" ->
      ((s, ss) => ss.copy(inDirPatterns = ss.inDirPatterns + s.r)),
    "in-ext" ->
      ((s, ss) => ss.copy(inExtensions = ss.addExtensions(s, ss.inExtensions))),
    "in-filepattern" ->
      ((s, ss) => ss.copy(inFilePatterns = ss.inFilePatterns + s.r)),
    "in-filetype" ->
      ((s, ss) => ss.copy(inFileTypes = ss.inFileTypes + FileType.forName(s))),
    "in-linesafterpattern" ->
      ((s, ss) => ss.copy(inLinesAfterPatterns  = ss.inLinesAfterPatterns + s.r)),
    "in-linesbeforepattern" ->
      ((s, ss) => ss.copy(inLinesBeforePatterns = ss.inLinesBeforePatterns + s.r)),
    "linesaftertopattern" ->
      ((s, ss) => ss.copy(linesAfterToPatterns  = ss.linesAfterToPatterns + s.r)),
    "linesafteruntilpattern" ->
      ((s, ss) => ss.copy(linesAfterUntilPatterns = ss.linesAfterUntilPatterns + s.r)),
    "maxlastmod" ->
      ((s, ss) => ss.copy(maxLastMod = ss.getLastModFromString(s))),
    "minlastmod" ->
      ((s, ss) => ss.copy(minLastMod = ss.getLastModFromString(s))),
    "out-archiveext" ->
      ((s, ss) => ss.copy(outArchiveExtensions = ss.addExtensions(s, ss.outArchiveExtensions))),
    "out-archivefilepattern" ->
      ((s, ss) => ss.copy(outArchiveFilePatterns = ss.outArchiveFilePatterns + s.r)),
    "out-dirpattern" ->
      ((s, ss) => ss.copy(outDirPatterns = ss.outDirPatterns + s.r)),
    "out-ext" ->
      ((s, ss) => ss.copy(outExtensions = ss.addExtensions(s, ss.outExtensions))),
    "out-filepattern" ->
      ((s, ss) => ss.copy(outFilePatterns = ss.outFilePatterns + s.r)),
    "out-filetype" ->
      ((s, ss) => ss.copy(outFileTypes = ss.outFileTypes + FileType.forName(s))),
    "out-linesafterpattern" ->
      ((s, ss) => ss.copy(outLinesAfterPatterns = ss.outLinesAfterPatterns + s.r)),
    "out-linesbeforepattern" ->
      ((s, ss) => ss.copy(outLinesBeforePatterns = ss.outLinesBeforePatterns + s.r)),
    "path" ->
      ((s, ss) => ss.copy(paths = ss.paths + Paths.get(s))),
    "searchpattern" ->
      ((s, ss) => ss.copy(searchPatterns = ss.searchPatterns + s.r)),
    "settings-file" ->
      ((s, ss) => settingsFromFile(s, ss)),
    "sort-by" ->
      ((s, ss) => ss.copy(sortBy = SortBy.forName(s))),
  )

  private type IntAction = (Int, SearchSettings) => SearchSettings

  private val intActionMap = Map[String, IntAction](
    "linesafter" -> ((i, ss) => ss.copy(linesAfter = i)),
    "linesbefore" -> ((i, ss) => ss.copy(linesBefore = i)),
    "maxdepth" -> ((i, ss) => ss.copy(maxDepth = i)),
    "maxlinelength" -> ((i, ss) => ss.copy(maxLineLength = i)),
    "mindepth" -> ((i, ss) => ss.copy(minDepth = i)),
  )

  private type LongAction = (Long, SearchSettings) => SearchSettings

  private val longActionMap = Map[String, LongAction](
    "maxsize" -> ((l, ss) => ss.copy(maxSize = l)),
    "minsize" -> ((l, ss) => ss.copy(minSize = l)),
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
    val jsonObject = new JSONObject(new JSONTokener(json))
    @tailrec
    def recSettingsFromJson(keys: List[String], settings: SearchSettings): SearchSettings = keys match {
      case Nil => settings
      case k :: ks =>
        val v = jsonObject.get(k)
        recSettingsFromJson(ks, applySetting(k, v, settings))
    }
    recSettingsFromJson(jsonObject.keySet().asScala.toList, ss)
  }

//  @tailrec
  private def applySetting(arg: String, obj: Any, ss: SearchSettings): SearchSettings = obj match {
    case b: Boolean =>
      if (this.boolActionMap.contains(arg)) {
        boolActionMap(arg)(b, ss)
      } else {
        throw new SearchException("Invalid option: " + arg)
      }
    case s: String =>
      if (this.stringActionMap.contains(arg)) {
        stringActionMap(arg)(s, ss)
      } else {
        throw new SearchException("Invalid option: " + arg)
      }
    case i: Int =>
      if (this.intActionMap.contains(arg)) {
        intActionMap(arg)(i, ss)
      } else if (this.longActionMap.contains(arg)) {
        longActionMap(arg)(i.toLong, ss)
      } else {
        throw new SearchException("Invalid option: " + arg)
      }
    case l: Long =>
      if (this.longActionMap.contains(arg)) {
        longActionMap(arg)(l, ss)
      } else if (this.intActionMap.contains(arg)) {
        intActionMap(arg)(l.toInt, ss)
      } else {
        throw new SearchException("Invalid option: " + arg)
      }
    case a: JSONArray =>
      applySettings(arg, a.toList.asScala.map(_.toString).toList, ss)
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
              if (boolActionMap.contains(longArg)) {
                if (Set("help", "version").contains(longArg)) {
                  nextArg(Nil, boolActionMap(longArg)(true, ss))
                } else {
                  nextArg(tail, boolActionMap(longArg)(true, ss))
                }
              } else if (stringActionMap.contains(longArg)
                         || intActionMap.contains(longArg)
                         || longActionMap.contains(longArg)) {
                if (tail.nonEmpty) {
                  if (stringActionMap.contains(longArg)) {
                    nextArg(tail.tail, stringActionMap(longArg)(tail.head, ss))
                  } else if (intActionMap.contains(longArg)) {
                    nextArg(tail.tail, intActionMap(longArg)(tail.head.toInt, ss))
                  } else {
                    nextArg(tail.tail, longActionMap(longArg)(tail.head.toLong, ss))
                  }
                } else {
                  throw new SearchException("Missing value for arg %s".format(arg))
                }
              } else {
                throw new SearchException("Invalid option: %s".format(arg))
              }
            case None =>
              throw new SearchException("Invalid option: %s".format(arg))
          }
        case arg :: tail =>
          nextArg(tail, ss.copy(paths = ss.paths + Paths.get(arg)))
      }
    }
    // default printResults to true since running as cli
    nextArg(args.toList, SearchSettings(printResults = true))
  }

  def usage(status: Int): Unit = {
    Common.log(getUsageString)
    sys.exit(status)
  }

  private def getUsageString: String = {
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
