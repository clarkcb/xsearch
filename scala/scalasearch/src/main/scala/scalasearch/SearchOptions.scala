package scalasearch

import org.json.{JSONArray, JSONException, JSONObject, JSONTokener}
import scalafind.{ArgOption, ArgToken, ArgTokenType, ArgTokenizer, Common, FileType, FileUtil, SortBy}

import java.io.{IOException, InputStreamReader}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class SearchOption(shortArg: Option[String], longArg: String, desc: String, argType: ArgTokenType) extends ArgOption {
  val sortArg: String = shortArg match {
    case Some(sa) => sa.toLowerCase + "@" + longArg.toLowerCase
    case None => longArg.toLowerCase
  }
}

object SearchOptions {
  private val _searchOptionsJsonPath = "/searchoptions.json"
  private val _searchOptions = mutable.ListBuffer.empty[SearchOption]

  private def searchOptions: List[SearchOption] = {
    if (_searchOptions.isEmpty) {
      loadSearchOptionsFromJson()
    }
    List.empty[SearchOption] ++ _searchOptions
  }

  private var _longArgMap = Map.empty[String, String]

  private def longArgMap: Map[String, String] = {
    if (_longArgMap.isEmpty) {
      val longOpts: Map[String, String] = searchOptions.map { o => (o.longArg, o.longArg)}.toMap
      val shortOpts = searchOptions.filter(_.shortArg.nonEmpty).map { o => (o.shortArg.get, o.longArg)}.toMap
      _longArgMap = longOpts ++ shortOpts ++ Map("path" -> "path")
    }
    _longArgMap
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
        val argType =
          if (boolActionMap.contains(longArg)) {
            ArgTokenType.Bool
          } else if (stringActionMap.contains(longArg)) {
            ArgTokenType.Str
          } else if (intActionMap.contains(longArg)) {
            ArgTokenType.Int
          } else if (longActionMap.contains(longArg)) {
            ArgTokenType.Long
          } else {
            throw new SearchException("Invalid option in searchoptions.json: " + longArg)
          }
        _searchOptions += SearchOption(shortArg, longArg, desc, argType)
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
    "nofollowsymlinks" -> ((b, ss) => ss.copy(followSymlinks = !b)),
    "noprintdirs" -> ((b, ss) => ss.copy(printDirs = !b)),
    "noprintfiles" -> ((b, ss) => ss.copy(printFiles = !b)),
    "noprintlines" -> ((b, ss) => ss.copy(printLines = !b)),
    "noprintmatches" -> ((b, ss) => ss.copy(printMatches = !b)),
    "noprintresults" -> ((b, ss) => ss.copy(printResults = !b)),
    "norecursive" -> ((b, ss) => ss.copy(recursive = !b)),
    "nosearcharchives" -> ((b, ss) => ss.copy(searchArchives = !b)),
    "printdirs" -> ((b, ss) => ss.copy(printDirs = b)),
    "printfiles" -> ((b, ss) => ss.copy(printFiles = b)),
    "printlines" -> ((b, ss) => ss.copy(printLines = b)),
    "printmatches" -> ((b, ss) => ss.copy(printMatches = b)),
    "printresults" -> ((b, ss) => ss.copy(printResults = b)),
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
      ((s, ss) => updateSettingsFromFile(ss, s)),
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

  @tailrec
  private def applySettings(arg: String, lst: List[Any], ss: SearchSettings): SearchSettings = lst match {
    case Nil => ss
    case h :: t => applySettings(arg, t, applySetting(arg, h, ss))
  }

  private def applySetting(arg: String, obj: Any, ss: SearchSettings): SearchSettings = {
    if (this.boolActionMap.contains(arg)) {
      obj match
        case b: Boolean =>
          boolActionMap(arg)(b, ss)
        case _ =>
          throw new SearchException("Invalid value for option: " + arg)
    } else if (this.stringActionMap.contains(arg)) {
      obj match
        case s: String =>
          stringActionMap(arg)(s, ss)
        case a: JSONArray =>
          applySettings(arg, a.toList.asScala.toList, ss)
        case _ =>
          throw new SearchException("Invalid value for option: " + arg)
    } else if (this.intActionMap.contains(arg)) {
      obj match
        case i: Int =>
          intActionMap(arg)(i, ss)
        case l: Long =>
          intActionMap(arg)(l.toInt, ss)
        case _ =>
          throw new SearchException("Invalid value for option: " + arg)
    } else if (this.longActionMap.contains(arg)) {
      obj match
        case i: Int =>
          longActionMap(arg)(i.toLong, ss)
        case l: Long =>
          longActionMap(arg)(l, ss)
        case _ =>
          throw new SearchException("Invalid value for option: " + arg)
    } else {
      throw new SearchException("Invalid option: " + arg)
    }
  }

  private def updateSettingsFromArgTokens(settings: SearchSettings, argTokens: List[ArgToken]): SearchSettings = {
    @tailrec
    def recSettingsFromArgTokens(argTokens: List[ArgToken], settings: SearchSettings): SearchSettings = argTokens match {
      case Nil => settings
      case a :: as =>
        recSettingsFromArgTokens(as, applySetting(a.name, a.value, settings))
    }

    recSettingsFromArgTokens(argTokens, settings)
  }

  private val argTokenizer: ArgTokenizer = new ArgTokenizer(searchOptions)

  def updateSettingsFromJson(settings: SearchSettings, json: String): SearchSettings = {
    try {
      val argTokens = argTokenizer.tokenizeJson(json)
      updateSettingsFromArgTokens(settings, argTokens)
    } catch {
      case e: JSONException =>
        throw new SearchException("Error parsing JSON")
    }
  }

  def updateSettingsFromFile(settings: SearchSettings, filePath: String): SearchSettings = {
    val argTokens = argTokenizer.tokenizeFile(filePath)
    updateSettingsFromArgTokens(settings, argTokens)
  }

  def updateSettingsFromArgs(settings: SearchSettings, args: Array[String]): SearchSettings = {
    val argTokens = argTokenizer.tokenizeArgs(args)
    updateSettingsFromArgTokens(settings, argTokens)
  }

  def settingsFromArgs(args: Array[String]): SearchSettings = {
    updateSettingsFromArgs(SearchSettings(printResults = true), args)
  }

  private def getUsageString: String = {
    val sb = new StringBuilder
    sb.append("Usage:\n")
    sb.append(" scalasearch [options] -s <searchpattern> <path> [<path> ...]\n\n")
    sb.append("Options:\n")
    val optPairs = searchOptions.sortWith(_.sortArg < _.sortArg).map { so =>
      val opts = so.shortArg match {
        case Some(sa) => s"-$sa,--${so.longArg}"
        case None => s"--${so.longArg}"
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

  def usage(status: Int): Unit = {
    Common.log(getUsageString)
    sys.exit(status)
  }
}
