import java.io._
import scala.io._

object Searcher {
    type OptionMap = Map[Symbol, Any]

    val NOSEARCH_EXTS  = """aif aifc aiff au avi bmp cab dmg eps gif
                            ico idlk ief iso jpe jpeg jpg
                            m3u m4a m4p mov movie mp3 mp4 mpe mpeg mpg mxu
                            ogg pdf pict png ps qt ra ram rm rpm
                            scc snd suo tif tiff wav""".split("""\s+""").toSet
    val COMPRESSED_EXTS = """bz2 cpio ear gz hqx jar pax rar sit sitx tar tgz
                             war zip Z""".split("""\s+""").toSet
    val BINARY_EXTS     = """ai bin class com dat dbmdl dcr dir dll dxr dms doc docx dot exe
                             hlp indd lnk mo obj pdb ppt psd pyc pyo qxd so swf sys
                             vsd xls xlsx xlt""".split("""\s+""").toSet
    val TEXT_EXTS       = """1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                             am app as asc ascx asm asp aspx bash bat bdsproj bsh
                             c cc cfg clj cls cmd cnt conf config cpp cs csh csproj css csv ctl
                             dat dbproj dbml dbschema ddl dep dfm disco dlg dof dpr dsp dsw dtd
                             env etx exp fls fs fsproj h hpp htm html ics iml in inc ini ipr iws
                             java js jsp layout log mak map master mht mxml
                             pas php php3 pl plist pm po properties py
                             rc rc2 rdf resx rex rtf rtx
                             scc sgm sgml sh sln smi smil spec sqc sql st str strings
                             suml svg sxw
                             t tcl tld tmx tsv txt url user
                             vb vbproj vbs vcf vcproj vdproj vm vrml vssscc vxml
                             wbxml webinfo wml wmls wrl wsd wsdd wsdl
                             xlf xml xsd xsl xslt""".split("""\s+""").toSet
    val UNKNOWN_EXTS    = """adm aps cli clw dat db def df2 ncb nt nt2 orig
                             pc plg roff sun t tex texinfo tr xwd""".split("""\s+""").toSet
    val ALL_EXTS = NOSEARCH_EXTS ++ COMPRESSED_EXTS ++ BINARY_EXTS ++ TEXT_EXTS ++ UNKNOWN_EXTS


    class SearchSettings(val inExtensions: Set[String], val outExtensions: Set[String],
                         val inDirPatterns: Set[String], val outDirPatterns: Set[String],
                         val inFilePatterns: Set[String], val outFilePatterns: Set[String],
                         val searchStrings: Set[String], val startpath: String) {
        override def toString() = {
            "SearchSettings(" +
            "startpath: " + startpath +
            "inExtensions: " + inExtensions + ", " +
            "outExtensions: " + outExtensions + ", " +
            "inDirPatterns: " + inDirPatterns + ", " +
            "outDirPatterns: " + outDirPatterns + ", " +
            "inFilePatterns: " + inFilePatterns + ", " +
            "outFilePatterns: " + outFilePatterns + ", " +
            "searchStrings: " + searchStrings + ", " +
            ")"
        }
    }

    def getSearchSettings(options: OptionMap): SearchSettings = {
        val inExtensions = options.getOrElse('x, Set()).asInstanceOf[Set[String]] //'
        val outExtensions = options.getOrElse('X, Set()).asInstanceOf[Set[String]] //'
        val inDirPatterns = options.getOrElse('d, Set()).asInstanceOf[Set[String]] //'
        val outDirPatterns = options.getOrElse('D, Set()).asInstanceOf[Set[String]] //'
        val inFilePatterns = options.getOrElse('f, Set()).asInstanceOf[Set[String]] //'
        val outFilePatterns = options.getOrElse('F, Set()).asInstanceOf[Set[String]] //'
        val searchStrings = options.getOrElse('s, Set()).asInstanceOf[Set[String]] //'
        val startpath = options.apply('startpath).toString //'
        new SearchSettings(inExtensions, outExtensions,
                           inDirPatterns, outDirPatterns,
                           inFilePatterns, outFilePatterns,
                           searchStrings, startpath)
    }

    class SearchResult(val searchString: String, val file: File, val lineNum: Int, val line: String) {
        override def toString() = {
            file.getPath + ": " + lineNum + ": " + line.trim
        }
    }

    def files(f: File): Iterable[File] = {
        if (f.isDirectory()) {
            f.listFiles.flatMap(child => files(child))
        } else {
            Seq(f)
        }
    }

    def hasMatchingExtension(f: File, searchSettings: SearchSettings): Boolean = {
        val ext = f.getName().split('.').last
        //if (!searchSettings.inExtensions.isEmpty && searchSettings.inExtensions.contains(ext)) {
        //    println("""Extension "%s" is a matching extension (found in inExtensions) """.format(ext))
        //} else if (!searchSettings.outExtensions.isEmpty && !searchSettings.outExtensions.contains(ext)) {
        //    println("""Extension "%s" is a matching extension (not found in outExtensions) """.format(ext))
        //}
        (searchSettings.inExtensions.isEmpty || searchSettings.inExtensions.contains(ext)) &&
        (searchSettings.outExtensions.isEmpty || !searchSettings.outExtensions.contains(ext))
    }

    def hasMatchingPattern(s: String, patterns: Set[String]): Boolean = {
        for (p <- patterns) {
            val m = p.r.findFirstIn(s)
            if (m != None) {
                //println("""String "%s" matches pattern "%s" """.format(s, p))
                return true
            }
        }
        false
    }

    def hasMatchingDirnamePattern(f: File, searchSettings: SearchSettings): Boolean = {
        if (!searchSettings.inDirPatterns.isEmpty) {
            hasMatchingPattern(f.getPath, searchSettings.inDirPatterns)
        } else if (!searchSettings.outDirPatterns.isEmpty) {
            !hasMatchingPattern(f.getPath, searchSettings.outDirPatterns)
        } else {
            true
        }
    }

    def hasMatchingFilenamePattern(f: File, searchSettings: SearchSettings): Boolean = {
        if (!searchSettings.inFilePatterns.isEmpty) {
            hasMatchingPattern(f.getName, searchSettings.inFilePatterns)
        } else if (!searchSettings.outFilePatterns.isEmpty) {
            !hasMatchingPattern(f.getName, searchSettings.outFilePatterns)
        } else {
            true
        }
    }

    def isTargetFile(f: File, searchSettings: SearchSettings, filterPredicates: List[Function2[File, SearchSettings, Boolean]]): Boolean = {
        for (pred <- filterPredicates) {
            if (!pred.apply(f, searchSettings)) {
                return false
            }
        }
        true
    }

    def getfilterPredicates(searchSettings: SearchSettings): List[(File,SearchSettings) => Boolean] = {
        var filterPredicates = List[(File,SearchSettings) => Boolean]()
        if (!searchSettings.inDirPatterns.isEmpty || !searchSettings.outDirPatterns.isEmpty) {
            val matchingDirnamePatternFunc = (f: File, searchSettings: SearchSettings) => hasMatchingDirnamePattern(f, searchSettings)
            filterPredicates ::= matchingDirnamePatternFunc
        }
        if (!searchSettings.inFilePatterns.isEmpty || !searchSettings.outFilePatterns.isEmpty) {
            val matchingFilenamePatternFunc = (f: File, searchSettings: SearchSettings) => hasMatchingFilenamePattern(f, searchSettings)
            filterPredicates ::= matchingFilenamePatternFunc
        }
        if (!searchSettings.inExtensions.isEmpty || !searchSettings.outExtensions.isEmpty) {
            val matchingExtensionFunc = (f: File, searchSettings: SearchSettings) => hasMatchingExtension(f, searchSettings)
            filterPredicates ::= matchingExtensionFunc
        }
        filterFuctions
    }

    def getSearchFiles(searchSettings: SearchSettings): Iterable[File] = {
        val filterPredicates = getfilterPredicates(searchSettings)
        val searchFiles = files(new File(searchSettings.startpath)) filter { f => isTargetFile(f, searchSettings, filterPredicates) }
        searchFiles
    }

    def searchTextFile(f: File, searchSettings: SearchSettings) = {
        println("Searching " + f.getPath)
        val lines = Source.fromFile(f.getAbsolutePath).getLines
        var lineNum: Int = 0
        for (line <- lines) {
            lineNum += 1
            for (s <- searchSettings.searchStrings) {
                val m = s.r.findFirstIn(line)
                if (m != None) {
                    val searchResult = new SearchResult(s, f, lineNum, line)
                    if (searchSettings.searchStrings.size > 1) {
                        print("\"" + searchResult.searchString + "\": ")
                    }
                    println(searchResult)
                }
            }
        }
    }

    def isBinaryFile(f: File) = {
        BINARY_EXTS.contains(f.getName.split('.').last)
    }

    def isTextFile(f: File) = {
        TEXT_EXTS.contains(f.getName.split('.').last)
    }

    def isSearchableFile(f: File) = {
        isTextFile(f) || isBinaryFile(f)
    }

    def searchFile(f: File, searchSettings: SearchSettings) = {
        if (isSearchableFile(f)) {
            if (isTextFile(f)) {
                searchTextFile(f, searchSettings)
            }
        }
    }

    def search(searchSettings: SearchSettings) = {
        val searchFiles = getSearchFiles(searchSettings)
        println("searchFiles: " + searchFiles)
        for (f <- searchFiles) {
            searchFile(f, searchSettings)
        }
    }

    def usage(status: Int) = {
        val usage = """
Usage:
search [-x ext[,ext]] [-f '<regex>'] -s '<searchstring>' <startpath>
        """
        println(usage)
        sys.exit(status)
    }

    def main(args: Array[String]) = {
        if (args.length == 0) usage(1)
        val arglist = args.toList

        def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
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
        }
        val options = nextOption(Map(),arglist)
        //println("options: " + options)
        val searchSettings = getSearchSettings(options)
        println("searchSettings: " + searchSettings)
        search(searchSettings)
    }
}
    