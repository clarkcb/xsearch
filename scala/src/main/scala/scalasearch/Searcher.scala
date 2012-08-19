package scalasearch

import java.io.File
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.util.matching.Regex

class Searcher (settings: SearchSettings) {

  val _fileMap = Map[File,ListBuffer[SearchResult]]()
  val _searchResults = ListBuffer[SearchResult]()
  val _timers = Map[String,Long]()

  def searchResults = _searchResults.toList

  val fileFilterPredicateDefinitions = List(
    (settings.inExtensions,
     (f: File) => settings.inExtensions.contains(FileUtil.getExtension(f))),
    (settings.outExtensions,
     (f: File) => !settings.outExtensions.contains(FileUtil.getExtension(f))),
    (settings.inDirPatterns,
     (f: File) => matchesAnyPattern(f.getPath, settings.inDirPatterns)),
    (settings.outDirPatterns,
     (f: File) => !matchesAnyPattern(f.getPath, settings.outDirPatterns)),
    (settings.inFilePatterns,
     (f: File) => matchesAnyPattern(f.getName, settings.inFilePatterns)),
    (settings.outFilePatterns,
     (f: File) => !matchesAnyPattern(f.getName, settings.outFilePatterns))
  )

  def getFileFilterPredicates(predicateDefs:List[Any]): List[(File) => Boolean] = {
    predicateDefs match {
      case (set:Set[Any], pred:((File) => Boolean)) :: tail =>
        if (!set.isEmpty) {
          pred :: getFileFilterPredicates(tail)
        } else {
          getFileFilterPredicates(tail)
        }
      case _ => List[(File) => Boolean]()
    }
  }

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (p => p.findFirstIn(s) != None)
  }

  def isTargetFile(f: File, predicates: List[(File) => Boolean]): Boolean = {
    predicates forall (_(f))
  }

  def files(f: File): Iterable[File] = {
    if (f.isDirectory) {
      f.listFiles.flatMap(child => files(child))
    } else {
      Seq(f)
    }
  }

  def getSearchFiles: Iterable[File] = {
    val predicates = getFileFilterPredicates(fileFilterPredicateDefinitions)
    val startdir = new File(settings.startpath)
    val searchFiles = files(startdir) filter { f => isTargetFile(f, predicates) }
    searchFiles
  }

  def listString(stringList:Iterable[Any]): String = {
    stringList.mkString("[\"", "\", \"", "\"]")
  }

  def startTimer(name:String) {
    val startTime = System.currentTimeMillis
    _timers.put(name+":start", startTime)
  }

  def stopTimer(name:String) {
    val stopTime = System.currentTimeMillis
    _timers.put(name+":stop", stopTime)
    val startTime = _timers(name+":start")
    val elapsed = stopTime - startTime
    println("Elapsed time for \"%s\": %d milliseconds".format(name, elapsed))
  }

  def search = {
    if (settings.dotiming) startTimer("getSearchFiles")
    val searchFiles = getSearchFiles
    if (settings.dotiming) stopTimer("getSearchFiles")
    if (settings.verbose) {
      println("searchFiles:\n" + listString(searchFiles))
    }
    if (settings.dotiming) startTimer("searchFiles")
    for (f <- searchFiles) {
      searchFile(f)
    }
    if (settings.printresults) println("%d results".format(_searchResults.length))
    if (settings.dotiming) stopTimer("searchFiles")
    if (settings.listfiles) printFileList
    if (settings.listlines) printLineList
  }

  def searchFile(f: File) = {
    if (FileUtil.isSearchableFile(f)) {
      if (FileUtil.isTextFile(f)) {
        searchTextFile(f)
      } else if (FileUtil.isBinaryFile(f)) {
        searchBinaryFile(f)
      }
    }
  }

  def searchTextFile(f: File) = {
    if (settings.verbose) {
      println("Searching text file " + f.getPath)
    }
    var stop = false
    val source = Source.fromFile(f.getAbsolutePath)
    val lines = source.getLines
    var lineNum: Int = 0
    while (lines.hasNext && !stop) {
      val line = lines.next
      lineNum += 1
      for (p <- settings.searchPatterns if p.findFirstIn(line) != None) {
        addSearchResult(new SearchResult(p, f, lineNum, line))
        if (settings.firstmatch &&
            _fileMap.contains(f) &&
            _fileMap(f).exists(_.searchPattern == p))
          stop = true
      }
    }
    source.close()
  }

  def searchBinaryFile(f: File) = {
    if (settings.verbose) {
      println("Searching binary file " + f.getPath)
    }
    val source = Source.fromFile(f.getAbsolutePath)
    val contents = source.mkString
    source.close()
    for (p <- settings.searchPatterns if p.findFirstIn(contents) != None) {
      addSearchResult(new SearchResult(p, f, 0, null))
    }
  }

  def addSearchResult(r: SearchResult) = {
    _searchResults.append(r)
    val resultListBuffer = _fileMap.getOrElse(r.file, ListBuffer[SearchResult]())
    resultListBuffer.append(r)
    _fileMap.put(r.file, resultListBuffer)
    if (settings.printresults) printSearchResult(r)
  }

  def printSearchResult(r: SearchResult) = {
    if (settings.searchPatterns.size > 1) {
      print("\"" + r.searchPattern + "\": ")
    }
    println(r)
  }

  def printFileList = {
    println("\nMatching files:")
    val files = _fileMap.keys.toList.sortWith(_.toString < _.toString)
    files.foreach(println(_))
  }

  def printLineList = {
    val lineset = Set[String]()
    for (r <- searchResults if r.line != null) {
      lineset.add(r.line.trim)
    }
    println("\nMatching lines:")
    val lines = lineset.toList.sortWith(_ < _)
    lines.foreach(println(_))
  }
}












