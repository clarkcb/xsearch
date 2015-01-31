package scalasearch

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite}

@RunWith(classOf[JUnitRunner])
class SearcherFilteringTest extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def getSettingsBuilder: SettingsBuilder = {
    val settings = new SettingsBuilder()
    settings.startPath = Some(".")
    settings.addSearchPattern("Searcher")
    settings
  }

  /** ***********************************************************
    * isSearchDir tests
    * ************************************************************/
  test("testisSearchDir_SingleDot_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File(".")))
  }

  test("test testisSearchDir_SingleDot_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File(".")))
  }

  test("testisSearchDir_DoubleDot_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File("..")))
  }

  test("testisSearchDir_IsHidden_False") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    assert(!searcher.isSearchDir(new File(".git")))
  }

  test("testisSearchDir_IsHiddenIncludeHidden_True") {
    val settings = getSettingsBuilder
    settings.excludeHidden = false
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File(".git")))
  }

  test("testisSearchDir_NoPatterns_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File("/Users")))
  }

  test("testisSearchDir_MatchesInPattern_True") {
    val settings = getSettingsBuilder
    settings.addInDirPattern("Search")
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File("CsSearch")))
  }

  test("testisSearchDir_MatchesOutPattern_False") {
    val settings = getSettingsBuilder
    settings.addOutDirPattern("Search")
    val searcher = new Searcher(settings.toSettings)
    assert(!searcher.isSearchDir(new File("CsSearch")))
  }

  test("testisSearchDir_DoesNotMatchInPattern_False") {
    val settings = getSettingsBuilder
    settings.addInDirPattern("SearchFiles")
    val searcher = new Searcher(settings.toSettings)
    assert(!searcher.isSearchDir(new File("CsSearch")))
  }

  test("testisSearchDir_DoesNotMatchOutPattern_True") {
    val settings = getSettingsBuilder
    settings.addOutDirPattern("SearchFiles")
    val searcher = new Searcher(settings.toSettings)
    val dir = new File("CsSearch")
    assert(searcher.isSearchDir(dir))
  }

  /** ***********************************************************
    * isSearchFile tests
    * ************************************************************/
  test("testIsSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesInExtension_True") {
    val settings = getSettingsBuilder
    settings.addInExtensions("cs")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchInExtension_False") {
    val settings = getSettingsBuilder
    settings.addInExtensions("java")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesOutExtension_False") {
    val settings = getSettingsBuilder
    settings.addOutExtensions("cs")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchOutExtension_True") {
    val settings = getSettingsBuilder
    settings.addOutExtensions("java")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesInPattern_True") {
    val settings = getSettingsBuilder
    settings.addInFilePattern("Search")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("Searcher.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchInPattern_False") {
    val settings = getSettingsBuilder
    settings.addInFilePattern("Search")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesOutPattern_False") {
    val settings = getSettingsBuilder
    settings.addOutFilePattern("Search")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("Searcher.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchOutPattern_True") {
    val settings = getSettingsBuilder
    settings.addOutFilePattern("Search")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  /** ***********************************************************
    * IsArchiveSearchFile tests
    * ************************************************************/
  test("testIsArchiveSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesInExtension_True") {
    val settings = getSettingsBuilder
    settings.addInArchiveExtensions("zip")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchInExtension_False") {
    val settings = getSettingsBuilder
    settings.addInArchiveExtensions("gz")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesOutExtension_False") {
    val settings = getSettingsBuilder
    settings.addOutArchiveExtensions("zip")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchOutExtension_True") {
    val settings = getSettingsBuilder
    settings.addOutArchiveExtensions("gz")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesInPattern_True") {
    val settings = getSettingsBuilder
    settings.addInArchiveFilePattern("arch")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchInPattern_False") {
    val settings = getSettingsBuilder
    settings.addInArchiveFilePattern("archives")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesOutPattern_False") {
    val settings = getSettingsBuilder
    settings.addOutArchiveFilePattern("arch")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchOutPattern_True") {
    val settings = getSettingsBuilder
    settings.addOutArchiveFilePattern("archives")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  /** ***********************************************************
    * FilterFile tests
    * ************************************************************/
  test("testFilterFile_IsHidden_False") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    val file = new File(".gitignore")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_IsHiddenIncludeHidden_True") {
    val settings = getSettingsBuilder
    settings.excludeHidden = false
    val searcher = new Searcher(settings.toSettings)
    val file = new File(".hidden.txt")
    println("searcher.isSearchFile(\"%s\"): %s".format(file, searcher.isSearchFile(file)))
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_ArchiveNoSearchArchives_False") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_ArchiveSearchArchives_True") {
    val settings = getSettingsBuilder
    settings.searchArchives = true
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_IsArchiveSearchFile_True") {
    val settings = getSettingsBuilder
    settings.searchArchives = true
    settings.addInArchiveExtensions("zip")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_NotIsArchiveSearchFile_False") {
    val settings = getSettingsBuilder
    settings.addOutExtensions("zip")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_ArchiveFileArchivesOnly_True") {
    val settings = getSettingsBuilder
    settings.archivesOnly = true
    val searcher = new Searcher(settings.toSettings)
    val file = new File("archive.zip")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_NoExtensionsNoPatterns_True") {
    val settings = getSettingsBuilder
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_IsSearchFile_True") {
    val settings = getSettingsBuilder
    settings.addInExtensions("cs")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_NotIsSearchFile_False") {
    val settings = getSettingsBuilder
    settings.addOutExtensions("cs")
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_NonArchiveFileArchivesOnly_False") {
    val settings = getSettingsBuilder
    settings.archivesOnly = true
    val searcher = new Searcher(settings.toSettings)
    val file = new File("FileUtil.cs")
    assert(!searcher.filterFile(file))
  }
}
