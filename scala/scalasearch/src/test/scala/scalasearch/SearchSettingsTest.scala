package scalasearch

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class SearchSettingsTest extends AnyFunSuite with BeforeAndAfterAll {

  def assertDefaultSettings(settings:SearchSettings): Unit = {
    assert(settings.archivesOnly == DefaultSearchSettings.archivesOnly)
    assert(settings.debug == DefaultSearchSettings.debug)
    assert(settings.firstMatch == DefaultSearchSettings.firstMatch)
    assert(settings.followSymlinks == DefaultSearchSettings.followSymlinks)
    assert(settings.includeHidden == DefaultSearchSettings.includeHidden)
    assert(settings.linesAfter == DefaultSearchSettings.linesAfter)
    assert(settings.linesBefore == DefaultSearchSettings.linesBefore)
    assert(settings.multiLineSearch == DefaultSearchSettings.multiLineSearch)
    assert(settings.printDirs == DefaultSearchSettings.printDirs)
    assert(settings.printFiles == DefaultSearchSettings.printFiles)
    assert(settings.printLines == DefaultSearchSettings.printLines)
    assert(settings.printResults == DefaultSearchSettings.printResults)
    assert(settings.printUsage == DefaultSearchSettings.printUsage)
    assert(settings.printVersion == DefaultSearchSettings.printVersion)
    assert(settings.searchArchives == DefaultSearchSettings.searchArchives)
    assert(settings.uniqueLines == DefaultSearchSettings.uniqueLines)
    assert(settings.verbose == DefaultSearchSettings.verbose)
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
