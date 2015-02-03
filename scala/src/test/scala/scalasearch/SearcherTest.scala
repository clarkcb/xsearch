package scalasearch

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite}
import org.scalatest.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class SearcherTest extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  val testFile1 = new File(getClass.getResource("/testFile1.txt").toURI)
  var lines1: Iterator[String] = Iterator.empty
  var contents1 = ""

  def getSettingsBuilder: SettingsBuilder = {
    val settingsBuilder = new SettingsBuilder()
    settingsBuilder.startPath =
      Some(System.getProperty("user.home") + "/src/git/xsearch/scala/src/test/resources")
    settingsBuilder.addSearchPattern("\\bSearcher\\b")
    settingsBuilder
  }

  override def beforeEach() {
    contents1 = Source.fromFile(testFile1).getLines().mkString("\n")
    lines1 = Source.fromFile(testFile1).getLines()
  }

  override def beforeAll() {
    beforeEach()
  }

  /*************************************************************
   * isSearchDir tests
   *************************************************************/
  test("testisSearchDir_SingleDot_True") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File(".")))
  }

  test("testisSearchDir_DoubleDot_True") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
    assert(searcher.isSearchDir(new File("..")))
  }

  test("testisSearchDir_IsHidden_False") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
    assert(!searcher.isSearchDir(new File(".git")))
  }

  test("testisSearchDir_IsHiddenIncludeHidden_True") {
    val settings = getSettingsBuilder
    settings.excludeHidden = false
    val searcher = new Searcher(settings.toSettings)
    assert(searcher.isSearchDir(new File(".git")))
  }

  test("testisSearchDir_NoPatterns_True") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
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

  /*************************************************************
   * isSearchFile tests
   *************************************************************/
  test("testIsSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
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

  /*************************************************************
   * IsArchiveSearchFile tests
   *************************************************************/
  test("testIsArchiveSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
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

  /*************************************************************
   * FilterFile tests
   *************************************************************/
  test("testFilterFile_IsHidden_False") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val file = new File(".gitignore")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_IsHiddenIncludeHidden_True") {
    val settings = getSettingsBuilder
    settings.excludeHidden = false
    val searcher = new Searcher(settings.toSettings)
    val file = new File(".hidden.txt")
    println("FileUtil.isHidden(\"%s\"): %s".format(file.getName,
      FileUtil.isHidden(file.getName)))
    assert(searcher.filterFile(file))
  }

  test("testFilterFile_ArchiveNoSearchArchives_False") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
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
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
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

  test("test getStartLineIndices") {
    val indices = Searcher.getLineIndices(contents1)
    println("contents (%d chars): \"%s\"".format(contents1.length, contents1))
    println("indices: "+indices)
  }

  /************************************************************
   * searchLineStringIterator tests
   *************************************************************/
  test("test searchLineStringIterator #1 - simple") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchLineStringIterator(lines1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results(0).line == "This is line 3, it includes the word Searcher")
    assert(results(0).lineNum == 3)
    assert(results(1).line == "Searcher")
    assert(results(1).lineNum == 7)
  }

  test("test searchLineStringIterator #2 - linesBefore+linesAfter") {
    val settingsBuilder = getSettingsBuilder
    settingsBuilder.linesBefore = 2
    settingsBuilder.linesAfter = 2
    val settings = settingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchLineStringIterator(lines1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test searchLineStringIterator #3 - inLinesBeforeAfterPattern") {
    val settingsBuilder = getSettingsBuilder
    settingsBuilder.linesBefore = 1
    settingsBuilder.linesAfter = 1
    settingsBuilder.addInLinesBeforePattern("line")
    settingsBuilder.addInLinesAfterPattern("line")
    val settings = settingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val lines1 = Source.fromFile(testFile1).getLines()
    val results = searcher.searchLineStringIterator(lines1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 1)
    assert(results.forall(r =>
      r.linesBefore.length == settings.linesBefore &&
        r.linesAfter.length == settings.linesAfter))
  }

  /************************************************************
   * searchMultiLineString tests
   *************************************************************/
  test("test searchMultiLineString #1 - simple") {
    val settings = getSettingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(lines1.mkString("\n"))
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results(0).line == "This is line 3, it includes the word Searcher")
    assert(results(1).line == "Searcher")
  }

  test("test searchMultiLineString #2 - linesBefore+linesAfter") {
    val settingsBuilder = getSettingsBuilder
    settingsBuilder.linesBefore = 2
    settingsBuilder.linesAfter = 2
    val settings = settingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(lines1.mkString("\n"))
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test searchMultiLineString #3 - inLinesBeforeAfterPattern") {
    val settingsBuilder = getSettingsBuilder
    settingsBuilder.linesBefore = 1
    settingsBuilder.linesAfter = 1
    settingsBuilder.addInLinesBeforePattern("line")
    settingsBuilder.addInLinesAfterPattern("line")
    val settings = settingsBuilder.toSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(lines1.mkString("\n"))
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 1)
    assert(results.forall(r =>
      r.linesBefore.length == settings.linesBefore &&
        r.linesAfter.length == settings.linesAfter))
  }
}
