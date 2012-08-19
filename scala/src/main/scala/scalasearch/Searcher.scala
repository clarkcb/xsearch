package scalasearch

import java.io.File
import scala.io._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.util.matching.Regex

class Searcher (settings: SearchSettings) {

  val _searchResults = ListBuffer[SearchResult]()

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
    predicates forall (p => p(f))
  }

  def files(f: File): Iterable[File] = {
    if (f.isDirectory()) {
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

  def search = {
    val searchFiles = getSearchFiles
    if (settings.verbose) {
      println("searchFiles:\n" + listString(searchFiles))
    }
    for (f <- searchFiles) {
      searchFile(f)
    }
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
    val source = Source.fromFile(f.getAbsolutePath)
    val lines = source.getLines
    var lineNum: Int = 0
    for (line <- lines) {
      lineNum += 1
      for (p <- settings.searchPatterns if p.findFirstIn(line) != None) {
        addSearchResult(new SearchResult(p, f, lineNum, line))
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
    if (settings.searchPatterns.size > 1) {
      print("\"" + r.searchPattern + "\": ")
    }
    println(r)
  }
}
