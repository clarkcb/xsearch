package scalasearch

import java.io._
import scala.io._

object Searcher {

    def files(f: File): Iterable[File] = {
        if (f.isDirectory()) {
            f.listFiles.flatMap(child => files(child))
        } else {
            Seq(f)
        }
    }

    def hasMatchingExtension(f: File, searchSettings: SearchSettings): Boolean = {
        val ext = f.getName().split('.').last
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
