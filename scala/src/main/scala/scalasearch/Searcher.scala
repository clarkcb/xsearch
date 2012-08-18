package scalasearch

import java.io.File
import scala.io._
import scala.collection.mutable.Set
import scala.util.matching.Regex

class Searcher (settings: SearchSettings) {

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    for (p <- patterns) {
      val m = p.findFirstIn(s)
      if (m != None) {
        return true
      }
    }
    false
  }

  def getFilterPredicates: List[(File) => Boolean] = {
    var predicates = List[(File) => Boolean]()
    if (!settings.inExtensions.isEmpty) {
      predicates =
        List((f: File) => settings.inExtensions.contains(FileUtil.getExtension(f))) ::: predicates
    }
    if (!settings.outExtensions.isEmpty) {
      predicates =
        List((f: File) => !settings.outExtensions.contains(FileUtil.getExtension(f))) ::: predicates
    }
    if (!settings.inDirPatterns.isEmpty) {
      predicates =
        List((f: File) => matchesAnyPattern(f.getPath, settings.inDirPatterns)) ::: predicates
    }
    if (!settings.outDirPatterns.isEmpty) {
      predicates =
        List((f: File) => !matchesAnyPattern(f.getPath, settings.outDirPatterns)) ::: predicates
    }
    if (!settings.inFilePatterns.isEmpty) {
      predicates =
        List((f: File) => matchesAnyPattern(f.getName, settings.inFilePatterns)) ::: predicates
    }
    if (!settings.outFilePatterns.isEmpty) {
      predicates =
        List((f: File) => !matchesAnyPattern(f.getName, settings.outFilePatterns)) ::: predicates
    }
    predicates
  }

  def isTargetFile(f: File, predicates: List[(File) => Boolean]): Boolean = {
    for (pred <- predicates) {
      if (!pred(f)) {
        return false
      }
    }
    true
  }

  def files(f: File): Iterable[File] = {
    if (f.isDirectory()) {
      f.listFiles.flatMap(child => files(child))
    } else {
      Seq(f)
    }
  }

  def getSearchFiles: Iterable[File] = {
    val filterPredicates = getFilterPredicates
    val searchFiles = files(new File(settings.startpath)) filter { f => isTargetFile(f, filterPredicates) }
    searchFiles
  }

  def searchTextFile(f: File) = {
    if (settings.debug) {
      println("Searching " + f.getPath)
    }
    val lines = Source.fromFile(f.getAbsolutePath).getLines
    var lineNum: Int = 0
    for (line <- lines) {
      lineNum += 1
      for (s <- settings.searchPatterns) {
        val m = s.findFirstIn(line)
        if (m != None) {
          val searchResult = new SearchResult(s, f, lineNum, line)
          if (settings.searchPatterns.size > 1) {
            print("\"" + searchResult.searchPattern + "\": ")
          }
          println(searchResult)
        }
      }
    }
  }

  def searchFile(f: File) = {
    if (FileUtil.isSearchableFile(f)) {
      if (FileUtil.isTextFile(f)) {
        searchTextFile(f)
      }
    }
  }

  def search = {
    val searchFiles = getSearchFiles
    if (settings.verbose) {
      println("searchFiles:\n" + searchFiles)
    }
    for (f <- searchFiles) {
      searchFile(f)
    }
  }
}
