package scalasearch

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

@RunWith(classOf[JUnitRunner])
class SearchSettingsTest extends FunSuite with BeforeAndAfterAll {

  def assertDefaultSettings(settings:SearchSettings) {
    assert(settings.archivesOnly == DefaultSettings.archivesOnly)
    assert(settings.debug == DefaultSettings.debug)
    assert(settings.doTiming == DefaultSettings.doTiming)
    assert(settings.excludeHidden == DefaultSettings.excludeHidden)
    assert(settings.firstMatch == DefaultSettings.firstMatch)
    assert(settings.linesAfter == DefaultSettings.linesAfter)
    assert(settings.linesBefore == DefaultSettings.linesBefore)
    assert(settings.listDirs == DefaultSettings.listDirs)
    assert(settings.listFiles == DefaultSettings.listFiles)
    assert(settings.listLines == DefaultSettings.listLines)
    assert(settings.multiLineSearch == DefaultSettings.multiLineSearch)
    assert(settings.printResults == DefaultSettings.printResults)
    assert(settings.printUsage == DefaultSettings.printUsage)
    assert(settings.printVersion == DefaultSettings.printVersion)
    assert(settings.searchArchives == DefaultSettings.searchArchives)
    assert(settings.uniqueLines == DefaultSettings.uniqueLines)
    assert(settings.verbose == DefaultSettings.verbose)
  }

  // test defaults
  test("""test default settings""") {
    val settings = SearchSettings()
    assertDefaultSettings(settings)
  }

  // test SettingsBuilder
  test("""test SettingsBuilder""") {
    val sb = new SettingsBuilder()
    sb.setArchivesOnly()
    sb.setDebug()
    sb.addInExtensions("java,scala")
    sb.addSearchPattern("Search")
    val settings = sb.toSettings
    assert(settings.archivesOnly)
    assert(settings.searchArchives)
    assert(settings.debug)
    assert(settings.verbose)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.contains("java"))
    assert(settings.inExtensions.contains("scala"))
    assert(settings.searchPatterns.size == 1)
    assert(settings.searchPatterns.toList.head.toString == "Search")
  }
}
