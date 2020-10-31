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

  def getSearchSettings: SearchSettings = {
    val startPath = System.getProperty("user.home") + "/src/xsearch/scala/scalasearch/src/test/resources"
    SearchSettings(startPath = Some(startPath), searchPatterns = Set("\\bSearcher\\b".r))
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

  /*************************************************************
   * isSearchFile tests
   *************************************************************/
  test("testIsSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesInExtension_True") {
    val settings = getSearchSettings.copy(inExtensions = Set("cs"))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchInExtension_False") {
    val settings = getSearchSettings.copy(inExtensions = Set("java"))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesOutExtension_False") {
    val settings = getSearchSettings.copy(outExtensions = Set("cs"))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchOutExtension_True") {
    val settings = getSearchSettings.copy(outExtensions = Set("java"))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesInPattern_True") {
    val settings = getSearchSettings.copy(inFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new File("Searcher.cs")
    assert(searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchInPattern_False") {
    val settings = getSearchSettings.copy(inFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_MatchesOutPattern_False") {
    val settings = getSearchSettings.copy(outFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new File("Searcher.cs")
    assert(!searcher.isSearchFile(file))
  }

  test("testIsSearchFile_DoesNotMatchOutPattern_True") {
    val settings = getSearchSettings.copy(outFilePatterns = Set("Search".r))
    val searcher = new Searcher(settings)
    val file = new File("FileUtil.cs")
    assert(searcher.isSearchFile(file))
  }

  /*************************************************************
   * IsArchiveSearchFile tests
   *************************************************************/
  test("testIsArchiveSearchFile_NoExtensionsNoPatterns_True") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesInExtension_True") {
    val settings = getSearchSettings.copy(inArchiveExtensions = Set("zip"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchInExtension_False") {
    val settings = getSearchSettings.copy(inArchiveExtensions = Set("gz"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesOutExtension_False") {
    val settings = getSearchSettings.copy(outArchiveExtensions = Set("zip"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchOutExtension_True") {
    val settings = getSearchSettings.copy(outArchiveExtensions = Set("gz"))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesInPattern_True") {
    val settings = getSearchSettings.copy(inArchiveFilePatterns = Set("arch".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchInPattern_False") {
    val settings = getSearchSettings.copy(inArchiveFilePatterns = Set("archives".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_MatchesOutPattern_False") {
    val settings = getSearchSettings.copy(outArchiveFilePatterns = Set("arch".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(!searcher.isArchiveSearchFile(file))
  }

  test("testIsArchiveSearchFile_DoesNotMatchOutPattern_True") {
    val settings = getSearchSettings.copy(outArchiveFilePatterns = Set("archives".r))
    val searcher = new Searcher(settings)
    val file = new File("archive.zip")
    assert(searcher.isArchiveSearchFile(file))
  }

  /*************************************************************
   * FilterFile tests
   *************************************************************/
  test("testFilterFile_IsHidden_False") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val file = new File(".gitignore")
    assert(!searcher.filterFile(file))
  }

  test("testFilterFile_IsHiddenIncludeHidden_True") {
    val settings = getSearchSettings.copy(excludeHidden = false)
    val searcher = new Searcher(settings)
    val file = new File(".hidden.txt")
    println("FileUtil.isHidden(\"%s\"): %s".format(file.getName,
      FileUtil.isHidden(file.getName)))
    assert(searcher.filterFile(file))
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

  test("test getStartLineIndices") {
    val indices = Searcher.getLineIndices(contents1)
    println("contents (%d chars): \"%s\"".format(contents1.length, contents1))
    println("indices: "+indices)
  }

  /************************************************************
   * searchLineStringIterator tests
   *************************************************************/
  test("test searchLineStringIterator #1 - simple") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchStringIterator(lines1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results.head.line == "This is line 3, it includes the word Searcher")
    assert(results.head.lineNum == 3)
    assert(results(1).line == "Searcher")
    assert(results(1).lineNum == 7)
  }

  test("test searchLineStringIterator #2 - linesBefore+linesAfter") {
    val settings = getSearchSettings.copy(linesBefore = 2, linesAfter = 2)
    val searcher = new Searcher(settings)
    val results = searcher.searchStringIterator(lines1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test searchLineStringIterator #3 - inLinesBeforeAfterPattern") {
    val settings = getSearchSettings.copy(linesBefore = 1, linesAfter = 1,
      inLinesBeforePatterns = Set("line".r), inLinesAfterPatterns = Set("line".r))
    val searcher = new Searcher(settings)
    val lines1 = Source.fromFile(testFile1).getLines()
    val results = searcher.searchStringIterator(lines1)
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
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(lines1.mkString("\n"))
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results(0).line == "This is line 3, it includes the word Searcher")
    assert(results(1).line == "Searcher")
  }

  test("test searchMultiLineString #2 - linesBefore+linesAfter") {
    val settings = getSearchSettings.copy(linesBefore = 2, linesAfter = 2)
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(lines1.mkString("\n"))
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test searchMultiLineString #3 - inLinesBeforeAfterPattern") {
    val settings = getSearchSettings.copy(linesBefore = 1, linesAfter = 1,
      inLinesBeforePatterns = Set("line".r), inLinesAfterPatterns = Set("line".r))
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(lines1.mkString("\n"))
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 1)
    assert(results.forall(r =>
      r.linesBefore.length == settings.linesBefore &&
        r.linesAfter.length == settings.linesAfter))
  }
}
