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

  def getSettingsBuilderOld: SettingsBuilder = {
    val settingsBuilder = new SettingsBuilder()
    settingsBuilder.startPath = System.getProperty("user.home") + "/src/git/xsearch"
    settingsBuilder.addInExtensions("scala")
    settingsBuilder.addSearchPattern("\\bSearcher\\b")
    settingsBuilder
  }

  def getSettingsBuilder: SettingsBuilder = {
    val settingsBuilder = new SettingsBuilder()
    settingsBuilder.startPath =
      System.getProperty("user.home") + "/src/git/xsearch/scala/src/test/resources"
    settingsBuilder.addInExtensions("txt")
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

  test("test getStartLineIndices") {
    val indices = Searcher.getLineIndices(contents1)
    println("contents (%d chars): \"%s\"".format(contents1.length, contents1))
    println("indices: "+indices)
  }

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
