package scalasearch

import org.scalatest.{BeforeAndAfterAll, FunSuite}
//import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

//@RunWith(classOf[JUnitRunner])
class SearchOptionsTest extends FunSuite with BeforeAndAfterAll{

  val startpath = "."
  val searchString = "Search"
  val requiredArgs = List("-s", searchString, startpath)

  def assertDefaultSettings(settings:SearchSettings) {
    val defaultSettings = SearchSettings()
    assert(settings.casesensitive == defaultSettings.casesensitive)
    assert(settings.debug == defaultSettings.debug)
    assert(settings.dotiming == defaultSettings.dotiming)
    assert(settings.firstmatch == defaultSettings.firstmatch)
    assert(settings.listfiles == defaultSettings.listfiles)
    assert(settings.listlines == defaultSettings.listlines)
    assert(settings.multilinesearch == defaultSettings.multilinesearch)
    assert(settings.printresults == defaultSettings.printresults)
    assert(settings.printusage == defaultSettings.printusage)
    assert(settings.printversion == defaultSettings.printversion)
    assert(settings.searchcompressed == defaultSettings.searchcompressed)
    assert(settings.verbose == defaultSettings.verbose)
  }

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

  test("""test settingsFromArgs with args="-x scala ." """) {
    val args = List("-x", "scala") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inExtensions: "+settings.inExtensions)
    assert(settings.inExtensions.size == 1)
    assert(settings.inExtensions.toList.head == "scala")
  }

  test("""test settingsFromArgs with args="-X scala ." """) {
    val args = List("-X", "scala") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.outExtensions: "+settings.outExtensions)
    assert(settings.outExtensions.size == 1)
    assert(settings.outExtensions.toList.head == "scala")
  }

  test("""test settingsFromArgs with args="-1 ." """) {
    val args = List("-1") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    assert(settings.firstmatch)
  }

  test("""test settingsFromArgs with args="-d scalasearch ." """) {
    val args = List("-d", "scalasearch") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inDirPatterns: "+settings.inDirPatterns)
    assert(settings.inDirPatterns.size == 1)
    assert(settings.inDirPatterns.toList.head.toString == "scalasearch")
  }

  test("""test settingsFromArgs with args="-f Search ." """) {
    val args = List("-f", "Search") ::: requiredArgs
    println("args: "+args)
    val settings = SearchOptions.settingsFromArgs(args)
    println("settings.inFilePatterns: "+settings.inFilePatterns)
    assert(settings.inFilePatterns.size == 1)
    assert(settings.inFilePatterns.toList.head.toString == "Search")
  }
}
