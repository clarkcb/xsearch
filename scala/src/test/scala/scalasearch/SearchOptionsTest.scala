package scalasearch

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SearchOptionsTest extends FunSuite with BeforeAndAfterAll {

  val startpath = "."
  val searchString = "Search"
  val requiredArgs = List("-s", searchString, startpath)

  def assertDefaultSettings(settings:SearchSettings) {
    val defaultSettings = SearchSettings()
    assert(settings.outDirPatterns.size == 3)
    assert(settings.outFilePatterns.size == 1)
    assert(settings.archivesOnly == defaultSettings.archivesOnly)
    assert(settings.debug == defaultSettings.debug)
    assert(settings.doTiming == defaultSettings.doTiming)
    assert(settings.firstMatch == defaultSettings.firstMatch)
    assert(settings.linesAfter == defaultSettings.linesAfter)
    assert(settings.linesBefore == defaultSettings.linesBefore)
    assert(settings.listDirs == defaultSettings.listDirs)
    assert(settings.listFiles == defaultSettings.listFiles)
    assert(settings.listLines == defaultSettings.listLines)
    assert(settings.multiLineSearch == defaultSettings.multiLineSearch)
    assert(settings.printResults == defaultSettings.printResults)
    assert(settings.printUsage == defaultSettings.printUsage)
    assert(settings.printVersion == defaultSettings.printVersion)
    assert(settings.searchArchives == defaultSettings.searchArchives)
    assert(settings.uniqueLines == defaultSettings.uniqueLines)
    assert(settings.verbose == defaultSettings.verbose)
  }

  // test default settings
  test("""test settingsFromArgs with requiredArgs""") {
    val args = requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.startpath: "+settings.startpath)
    assertDefaultSettings(settings)
    assert(settings.startpath == startpath)
    assert(settings.searchPatterns.size == 1)
    assert(settings.searchPatterns.toList.head.toString == searchString)
  }

  // test -a / --allmatches
  test("""test settingsFromArgs with args="-a" / "--allmatches" """) {
    val shortArgs = List("-a") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.firstMatch)

    val longArgs = List("--allmatches") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(!longSettings.firstMatch)
  }

  // test --archivesonly
  test("""test settingsFromArgs with args="--archivesonly" """) {
    val args = List("--archivesonly") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    assert(settings.archivesOnly)
  }

  // test --debug
  test("""test settingsFromArgs with args="--debug" """) {
    val args = List("--debug") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    assert(settings.debug)
  }

  // test -t / --dotiming
  test("""test settingsFromArgs with args="-t" / "--dotiming" """) {
    val shortArgs = List("-t") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.doTiming)

    val longArgs = List("--dotiming") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.doTiming)
  }

  // test -a / --firstmatch
  test("""test settingsFromArgs with args="-1" / "--firstmatch" """) {
    val shortArgs = List("-1") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.firstMatch)

    val longArgs = List("--firstmatch") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.firstMatch)
  }

  // test -h / --help
  test("""test settingsFromArgs with args="-h" / "--help" """) {
    val shortArgs = List("-h") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printUsage)

    val longArgs = List("--help") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.printUsage)
  }

  // test --in-archivefilepattern
  test("""test settingsFromArgs with args="--in-archivefilepattern search" """) {
    val args = List("--in-archivefilepattern", "search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inArchiveFilePatterns: "+settings.inArchiveFilePatterns)
    assert(settings.inArchiveFilePatterns.size == 1)
    assert(settings.inArchiveFilePatterns.map(_.toString()).contains("search"))
  }

  // test -d / --in-dirpattern
  test("""test settingsFromArgs with args="-d search" """) {
    val shortArgs = List("-d", "search") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inDirPatterns: "+shortSettings.inDirPatterns)
    assert(shortSettings.inDirPatterns.size == 1)
    assert(shortSettings.inDirPatterns.map(_.toString()).contains("search"))

    val longArgs = List("--in-dirpattern", "search") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    println("longSettings.inDirPatterns: "+longSettings.inDirPatterns)
    assert(longSettings.inDirPatterns.size == 1)
    assert(longSettings.inDirPatterns.map(_.toString()).contains("search"))
  }

  // test -x / --in-ext
  test("""test settingsFromArgs with args="-x scala" """) {
    val shortArgs = List("-x", "scala") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inExtensions: "+shortSettings.inExtensions)
    assert(shortSettings.inExtensions.size == 1)
    assert(shortSettings.inExtensions.toList.head == "scala")
    assert(shortSettings.outExtensions.size == 0)

    val longArgs = List("--in-ext", "scala") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    println("longSettings.inExtensions: "+longSettings.inExtensions)
    assert(longSettings.inExtensions.size == 1)
    assert(longSettings.inExtensions.toList.head == "scala")
    assert(longSettings.outExtensions.size == 0)
  }

  // test -x with comma-separated list of exts
  test("""test settingsFromArgs with args="-x java,scala" """) {
    val args = List("-x", "java,scala") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inExtensions: "+settings.inExtensions)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.toList.head == "java")
    assert(settings.inExtensions.toList.last == "scala")
    assert(settings.outExtensions.size == 0)
  }

  // test -f / --in-filepattern
  test("""test settingsFromArgs with args="-f Search" """) {
    val shortArgs = List("-f", "Search") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    println("shortSettings.inFilePatterns: "+shortSettings.inFilePatterns)
    assert(shortSettings.inFilePatterns.size == 1)
    assert(shortSettings.inFilePatterns.map(_.toString()).contains("Search"))

    val longArgs = List("--in-filepattern", "Search") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    println("longSettings.inFilePatterns: "+longSettings.inFilePatterns)
    assert(longSettings.inFilePatterns.size == 1)
    assert(longSettings.inFilePatterns.map(_.toString()).contains("Search"))
  }

  // test --in-linesafterpattern
  test("""test settingsFromArgs with args="--in-linesafterpattern Search" """) {
    val args = List("--in-linesafterpattern", "Search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inLinesAfterPatterns: "+settings.inLinesAfterPatterns)
    assert(settings.inLinesAfterPatterns.size == 1)
    assert(settings.inLinesAfterPatterns.map(_.toString()).contains("Search"))
  }

  // test --in-linesbeforepattern
  test("""test settingsFromArgs with args="--in-linesbeforepattern Search" """) {
    val args = List("--in-linesbeforepattern", "Search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inLinesBeforePatterns: "+settings.inLinesBeforePatterns)
    assert(settings.inLinesBeforePatterns.size == 1)
    assert(settings.inLinesBeforePatterns.map(_.toString()).contains("Search"))
  }

  // test -B / --linesafter
  test("""test settingsFromArgs with args="-B 2" / "--linesafter 2" """) {
    val shortArgs = List("-B", "2") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.linesAfter == 2)

    val longArgs = List("--linesafter", "2") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.linesAfter == 2)
  }

  // test --linesaftertopattern
  test("""test settingsFromArgs with args="--linesaftertopattern ^\]$" """) {
    val args = List("--linesaftertopattern", "^\\]$") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.linesAfterToPatterns: "+settings.linesAfterToPatterns)
    assert(settings.linesAfterToPatterns.size == 1)
    assert(settings.linesAfterToPatterns.toList.head.toString == "^\\]$")
  }

  // test --linesafteruntilpattern
  test("""test settingsFromArgs with args="--linesafteruntilpattern ^\]$" """) {
    val args = List("--linesafteruntilpattern", "^\\]$") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.linesAfterUntilPatterns: "+settings.linesAfterUntilPatterns)
    assert(settings.linesAfterUntilPatterns.size == 1)
    assert(settings.linesAfterUntilPatterns.toList.head.toString == "^\\]$")
  }

  // test -b / --linesbefore
  test("""test settingsFromArgs with args="-b 2" / "--linesbefore 2" """) {
    val shortArgs = List("-b", "2") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.linesBefore == 2)

    val longArgs = List("--linesbefore", "2") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.linesBefore == 2)
  }

  // test --listdirs
  test("""test settingsFromArgs with args="--listdirs" """) {
    val args = List("--listdirs") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    assert(settings.listDirs)
  }

  // test --listfiles
  test("""test settingsFromArgs with args="--listfiles" """) {
    val args = List("--listfiles") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    assert(settings.listFiles)
  }

  // test --listlines
  test("""test settingsFromArgs with args="--listlines" """) {
    val args = List("--listlines") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    assert(settings.listLines)
  }

  // test -m / --multilinesearch
  test("""test settingsFromArgs with args="-m" / "--multilinesearch" """) {
    val shortArgs = List("-m") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.multiLineSearch)

    val longArgs = List("--multilinesearch") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.multiLineSearch)
  }

  // test -P / --noprintmatches
  test("""test settingsFromArgs with args="-P" / "--noprintmatches" """) {
    val shortArgs = List("-P") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.printResults)

    val longArgs = List("--noprintmatches") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(!longSettings.printResults)
  }

  // test -Z / --nosearcharchives
  test("""test settingsFromArgs with args="-Z" / "--nosearcharchives" """) {
    val shortArgs = List("-Z") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(!shortSettings.searchArchives)

    val longArgs = List("--nosearcharchives") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(!longSettings.searchArchives)
  }

  // test --out-archivefilepattern
  test("""test settingsFromArgs with args="--out-archivefilepattern search" """) {
    val args = List("--out-archivefilepattern", "search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.outArchiveFilePatterns: "+settings.outArchiveFilePatterns)
    assert(settings.outArchiveFilePatterns.size == 1)
    assert(settings.outArchiveFilePatterns.map(_.toString()).contains("search"))
  }

  // test -D / --out-dirpattern
  test("""test settingsFromArgs with args="-D search" """) {
    val shortArgs = List("-D", "search") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outDirPatterns: "+shortSettings.outDirPatterns)
    assert(shortSettings.outDirPatterns.size ==
      DefaultSettings.outDirPatterns.size + 1)
    assert(shortSettings.outDirPatterns.map(_.toString()).contains("search"))

    val longArgs = List("--out-dirpattern", "search") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    println("longSettings.outDirPatterns: "+longSettings.outDirPatterns)
    assert(longSettings.outDirPatterns.size ==
      DefaultSettings.outDirPatterns.size + 1)
    assert(longSettings.outDirPatterns.map(_.toString()).contains("search"))
  }

  // test -X / --out-ext
  test("""test settingsFromArgs with args="-X scala" """) {
    val shortArgs = List("-X", "scala") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outExtensions: "+shortSettings.outExtensions)
    assert(shortSettings.inExtensions.size == 0)
    assert(shortSettings.outExtensions.size == 1)
    assert(shortSettings.outExtensions.map(_.toString).contains("scala"))

    val longArgs = List("--out-ext", "scala") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    println("longSettings.inExtensions: "+longSettings.inExtensions)
    assert(longSettings.inExtensions.size == 0)
    assert(longSettings.outExtensions.size == 1)
    assert(longSettings.outExtensions.map(_.toString).contains("scala"))
  }

  // test -X with comma-separated list of exts
  test("""test settingsFromArgs with args="-X java,scala" """) {
    val args = List("-X", "java,scala") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.outExtensions: "+settings.outExtensions)
    assert(settings.inExtensions.size == 0)
    assert(settings.outExtensions.size == 2)
    assert(settings.outExtensions.contains("java"))
    assert(settings.outExtensions.contains("scala"))
  }

  // test -F / --out-filepattern
  test("""test settingsFromArgs with args="-F Search" """) {
    val shortArgs = List("-F", "Search") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    println("shortSettings.outFilePatterns: "+shortSettings.outFilePatterns)
    assert(shortSettings.outFilePatterns.size ==
      DefaultSettings.outFilePatterns.size + 1)
    assert(shortSettings.outFilePatterns.map(_.toString()).contains("Search"))

    val longArgs = List("--out-filepattern", "Search") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    println("longSettings.outFilePatterns: "+longSettings.outFilePatterns)
    assert(longSettings.outFilePatterns.size ==
      DefaultSettings.outFilePatterns.size + 1)
    assert(longSettings.outFilePatterns.map(_.toString()).contains("Search"))
  }

  // test --out-linesafterpattern
  test("""test settingsFromArgs with args="--out-linesafterpattern Search" """) {
    val args = List("--out-linesafterpattern", "Search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.outLinesAfterPatterns: "+settings.outLinesAfterPatterns)
    assert(settings.outLinesAfterPatterns.size == 1)
    assert(settings.outLinesAfterPatterns.toList.head.toString == "Search")
  }

  // test --out-linesbeforepattern
  test("""test settingsFromArgs with args="--out-linesbeforepattern Search" """) {
    val args = List("--out-linesbeforepattern", "Search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.outLinesBeforePatterns: "+settings.outLinesBeforePatterns)
    assert(settings.outLinesBeforePatterns.size == 1)
    assert(settings.outLinesBeforePatterns.toList.head.toString == "Search")
  }

  // test -p / --printmatches
  test("""test settingsFromArgs with args="-p" / "--printmatches" """) {
    val shortArgs = List("-p") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printResults)

    val longArgs = List("--printmatches") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.printResults)
  }

  // test -z / --searcharchives
  test("""test settingsFromArgs with args="-z" / "--searcharchives" """) {
    val shortArgs = List("-z") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.searchArchives)

    val longArgs = List("--searcharchives") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.searchArchives)
  }

  // test -u / --uniquelines
  test("""test settingsFromArgs with args="-u" / "--uniquelines" """) {
    val shortArgs = List("-u") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.uniqueLines)

    val longArgs = List("--uniquelines") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.uniqueLines)
  }

  // test -v / --verbose
  test("""test settingsFromArgs with args="-v" / "--verbose" """) {
    val shortArgs = List("-v") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.verbose)

    val longArgs = List("--verbose") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.verbose)
  }

  // test -V / --version
  test("""test settingsFromArgs with args="-V" / "--version" """) {
    val shortArgs = List("-V") ::: requiredArgs
    println("shortArgs: "+shortArgs)
    val shortSettings = SearchOptions.settingsFromArgs(shortArgs)
    assert(shortSettings.printVersion)

    val longArgs = List("--version") ::: requiredArgs
    println("longArgs: "+longArgs)
    val longSettings = SearchOptions.settingsFromArgs(longArgs)
    assert(longSettings.printVersion)
  }
}
