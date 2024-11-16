package scalasearch

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import scalafind.FileUtil

import java.io.File
import java.nio.file.Paths
import scala.io.Source

class SearcherTest extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  val testFile1 = new File(getClass.getResource("/testFile1.txt").toURI)
  var lines1: Iterator[String] = Iterator.empty
  var contents1 = ""

  def getSearchSettings: SearchSettings = {
    val startPath = System.getProperty("user.home") + "/src/xsearch/scala/scalasearch/src/test/resources"
    SearchSettings(paths = Set(Paths.get(startPath)), searchPatterns = Set("\\bSearcher\\b".r))
  }

  def getFileLines(file: File): Iterator[String] = {
    val source = Source.fromFile(file)
    val lines = source.getLines()
    source.close()
    lines
  }

  override def beforeEach(): Unit = {
    contents1 = FileUtil.getFileContents(testFile1)
    lines1 = getFileLines(testFile1)
  }

  override def beforeAll(): Unit = {
    beforeEach()
  }

  /************************************************************
   * searchLineStringIterator tests
   *************************************************************/
  test("test searchLineStringIterator #1 - simple") {
    val settings = getSearchSettings
    val searcher = new Searcher(settings)
    val source = Source.fromFile(testFile1)
    val lines = source.getLines()
    val results = searcher.searchStringIterator(lines)
    source.close()
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results.head.line == Some("This is line 3, it includes the word Searcher"))
    assert(results.head.lineNum == 3)
    assert(results(1).line == Some("Searcher"))
    assert(results(1).lineNum == 7)
  }

  test("test searchLineStringIterator #2 - linesBefore+linesAfter") {
    val settings = getSearchSettings.copy(linesBefore = 2, linesAfter = 2)
    val searcher = new Searcher(settings)
    val source = Source.fromFile(testFile1)
    val lines = source.getLines()
    val results = searcher.searchStringIterator(lines)
    source.close()
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test searchLineStringIterator #3 - inLinesBeforeAfterPattern") {
    val settings = getSearchSettings.copy(linesBefore = 1, linesAfter = 1,
      inLinesBeforePatterns = Set("line".r), inLinesAfterPatterns = Set("line".r))
    val searcher = new Searcher(settings)
    val source = Source.fromFile(testFile1)
    val lines = source.getLines()
    val results = searcher.searchStringIterator(lines)
    source.close()
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
    val results = searcher.searchMultiLineString(contents1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.isEmpty && r.linesAfter.isEmpty))
    assert(results(0).line == Some("This is line 3, it includes the word Searcher"))
    assert(results(1).line == Some("Searcher"))
  }

  test("test searchMultiLineString #2 - linesBefore+linesAfter") {
    val settings = getSearchSettings.copy(linesBefore = 2, linesAfter = 2)
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(contents1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 2)
    assert(results.forall(r => r.linesBefore.length == 2 && r.linesAfter.length == 2))
  }

  test("test searchMultiLineString #3 - inLinesBeforeAfterPattern") {
    val settings = getSearchSettings.copy(linesBefore = 1, linesAfter = 1,
      inLinesBeforePatterns = Set("line".r), inLinesAfterPatterns = Set("line".r))
    val searcher = new Searcher(settings)
    val results = searcher.searchMultiLineString(contents1)
    println("results (%d):\n%s".format(results.length, results.mkString("\n")))
    assert(results.length == 1)
    assert(results.forall(r =>
      r.linesBefore.length == settings.linesBefore &&
        r.linesAfter.length == settings.linesAfter))
  }
}
