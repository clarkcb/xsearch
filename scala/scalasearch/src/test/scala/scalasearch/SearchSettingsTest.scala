package scalasearch

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class SearchSettingsTest extends AnyFunSuite with BeforeAndAfterAll {

  def assertDefaultSettings(settings:SearchSettings): Unit = {
    assert(settings.archivesOnly == DefaultSettings.archivesOnly)
    assert(settings.debug == DefaultSettings.debug)
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
    val settings = SearchSettings(archivesOnly = true, debug = true,
      inExtensions = Set("java", "scala"), searchPatterns = Set("Search".r))
    assert(settings.archivesOnly)
    //assert(settings.searchArchives)
    assert(settings.debug)
    //assert(settings.verbose)
    assert(settings.inExtensions.size == 2)
    assert(settings.inExtensions.contains("java"))
    assert(settings.inExtensions.contains("scala"))
    assert(settings.searchPatterns.size == 1)
    assert(settings.searchPatterns.toList.head.toString == "Search")
  }
}
