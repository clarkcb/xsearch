package scalasearch

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File

class SearcherFilteringTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  def getSearchSettings: SearchSettings = {
    SearchSettings(paths = Set("."), searchPatterns = Set("Searcher".r))
  }

  /** ***********************************************************
    * isSearchDir tests
    * ************************************************************/
  test("testisSearchDir_SingleDot_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File(".")))
  }

  test("test testisSearchDir_SingleDot_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File(".")))
  }

  test("testisSearchDir_DoubleDot_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File("..")))
  }

  test("testisSearchDir_IsHidden_False") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    assert(!searcher.isSearchDir(new File(".git")))
  }

  test("testisSearchDir_IsHiddenIncludeHidden_True") {
    val settings = getSearchSettings.copy(excludeHidden = false)
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File(".git")))
  }

  test("testisSearchDir_NoPatterns_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File("/Users")))
  }

  test("testisSearchDir_MatchesInPattern_True") {
    val settings = getSearchSettings.copy(inDirPatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File("CsSearch")))
  }

  test("testisSearchDir_MatchesOutPattern_False") {
    val settings = getSearchSettings.copy(outDirPatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    assert(!searcher.isSearchDir(new File("CsSearch")))
  }

  test("testisSearchDir_DoesNotMatchInPattern_False") {
    val settings = getSearchSettings.copy(inDirPatterns = Set("SearchFiles".r))
    val searcher = new Searcher(settings)
    assert(!searcher.isSearchDir(new File("CsSearch")))
  }

  test("testisSearchDir_DoesNotMatchOutPattern_True") {
    val settings = getSearchSettings.copy(outDirPatterns = Set("SearchFiles".r))
    val searcher = new Searcher(settings)
    val dir = new File("CsSearch")
    assert(searcher.isSearchDir(dir))
  }

  /** ***********************************************************
    * isSearchFile tests
    * ************************************************************/
  test("testIsSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesInExtension_True") {
    val settings = getSearchSettings.copy(inExtensions = Set("cs"))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchInExtension_False") {
    val settings = getSearchSettings.copy(inExtensions = Set("java"))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesOutExtension_False") {
    val settings = getSearchSettings.copy(outExtensions = Set("cs"))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchOutExtension_True") {
    val settings = getSearchSettings.copy(outExtensions = Set("java"))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesInPattern_True") {
    val settings = getSearchSettings.copy(inFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("Searcher.cs"), FileType.Code)
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchInPattern_False") {
    val settings = getSearchSettings.copy(inFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesOutPattern_False") {
    val settings = getSearchSettings.copy(outFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new SearchFile(new File("Searcher.cs"), FileType.Code)
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchOutPattern_True") {
    val settings = getSearchSettings.copy(outFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
//    val file = new File("FileUtil.cs")
    val file = new SearchFile(new File("FileUtil.cs"), FileType.Code)
    assert(searcher.isSearchFile(file))
  }

  /** ***********************************************************
    * IsArchiveSearchFile tests
    * ************************************************************/
  test("testIsArchiveSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_MatchesInExtension_True") {
    val settings = getSearchSettings.copy(inArchiveExtensions = Set("zip"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_DoesNotMatchInExtension_False") {
    val settings = getSearchSettings.copy(inArchiveExtensions = Set("gz"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_MatchesOutExtension_False") {
    val settings = getSearchSettings.copy(outArchiveExtensions = Set("zip"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_DoesNotMatchOutExtension_True") {
    val settings = getSearchSettings.copy(outArchiveExtensions = Set("gz"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_MatchesInPattern_True") {
    val settings = getSearchSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_DoesNotMatchInPattern_False") {
    val settings = getSearchSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_MatchesOutPattern_False") {
    val settings = getSearchSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file.getName))
  }

  test("testIsArchiveSearchFile_DoesNotMatchOutPattern_True") {
    val settings = getSearchSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file.getName))
  }

  /** ***********************************************************
    * FilterFile tests
    * ************************************************************/
  test("testFilterFile_IsHidden_False") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File(".gitignore")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_IsHiddenIncludeHidden_True") {
    val settings = getSearchSettings.copy(excludeHidden = false)
    val searcher = new Searcher(settings)
    val searchFile = new SearchFile(new File(".hidden.txt"), FileType.Text)
    println("searcher.isSearchFile(\"%s\"): %s".format(searchFile, searcher.isSearchFile(searchFile)))
    assert(searcher.filterFile(searchFile.file))
  }

  test("testFilterFile_ArchiveNoSearchArchives_False") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_ArchiveSearchArchives_True") {
    val settings = getSearchSettings.copy(searchArchives = true)
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_IsArchiveSearchFile_True") {
    val settings = getSearchSettings.copy(searchArchives = true,
      inArchiveExtensions = Set("zip"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_NotIsArchiveSearchFile_False") {
    val settings = getSearchSettings.copy(outExtensions = Set("zip"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_ArchiveFileArchivesOnly_True") {
    val settings = getSearchSettings.copy(archivesOnly = true)
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_NoExtensionsNoPatterns_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_IsSearchFile_True") {
    val settings = getSearchSettings.copy(inExtensions = Set("cs"))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_NotIsSearchFile_False") {
    val settings = getSearchSettings.copy(outExtensions = Set("cs"))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_NonArchiveFileArchivesOnly_False") {
    val settings = getSearchSettings.copy(archivesOnly = true)
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(!searcher.filterFile(file))
  }
}
