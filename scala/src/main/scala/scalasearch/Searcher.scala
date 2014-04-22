package scalasearch

import java.io.File
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

class Searcher (settings: SearchSettings) {
  def validateSettings() {
    assert(settings.startpath.nonEmpty, "Missing startpath")
    assert(settings.searchPatterns.size > 0, "No search patterns defined")
  }
  validateSettings()

  if (settings.debug) {
    println("settings: "+settings)
  }

  val _fileMap = mutable.Map[File, mutable.ListBuffer[SearchResult]]()
  val _searchResults = mutable.ListBuffer[SearchResult]()
  val _timers = mutable.Map[String,Long]()

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
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def anyMatchesAnyPattern(strings: Iterable[String], patterns: Set[Regex]): Boolean = {
    strings exists (matchesAnyPattern(_, patterns))
  }

  def isSearchDir(d: File): Boolean = {
    if (settings.inDirPatterns.nonEmpty && !matchesAnyPattern(d.getName, settings.inDirPatterns)) {
      false
    } else if (settings.outDirPatterns.nonEmpty && matchesAnyPattern(d.getName, settings.outDirPatterns)) {
      false
    } else {
      true
    }
  }

  def getSearchDirs(startDir:File): Iterable[File] = {
    if (settings.verbose) println("getSearchDirs(%s)".format(startDir.toString))
    def getFilteredDirs(dir:File): Iterable[File] = {
      val filteredDirs = dir.listFiles.filter(_.isDirectory).filter(isSearchDir)
      filteredDirs ++ filteredDirs.flatMap(getFilteredDirs)
    }
    getFilteredDirs(startDir)
  }

  def isSearchFile(f: File): Boolean = {
    if (settings.inExtensions.nonEmpty && !settings.inExtensions.contains(FileUtil.getExtension(f))) {
      false
    } else if (settings.outExtensions.nonEmpty && settings.outExtensions.contains(FileUtil.getExtension(f))) {
      false
    } else if (settings.inFilePatterns.nonEmpty && !matchesAnyPattern(f.getName, settings.inFilePatterns)) {
      false
    } else if (settings.outFilePatterns.nonEmpty && matchesAnyPattern(f.getName, settings.outFilePatterns)) {
      false
    } else {
      true
    }
  }

  def getSearchFiles(searchDirs:Iterable[File]): Iterable[File] = {
    def getFilteredFiles(dirs:Iterable[File]): Iterable[File] = {
      dirs.size match {
        case 0 => Nil
        case _ =>
          val filteredFiles = dirs.head.listFiles().filterNot(_.isDirectory).filter(isSearchFile)
          filteredFiles ++ getFilteredFiles(dirs.tail)
      }
    }
    getFilteredFiles(searchDirs)
  }

  def listToString(stringList:Iterable[Any]): String = {
    stringList.mkString("[\"", "\", \"", "\"]")
  }

  def addTimer(name:String, action:String) {
    _timers.put(name+":"+action, System.currentTimeMillis)
  }

  def startTimer(name:String) {
    addTimer(name, "start")
  }

  def getElapsed(name:String): Long = {
    val startTime = _timers(name+":start")
    val stopTime = _timers(name+":stop")
    stopTime - startTime
  }

  def printElapsed(name:String) {
    val elapsed = getElapsed(name)
    println("Elapsed time for \"%s\": %d milliseconds".format(name, elapsed))
  }

  def stopTimer(name:String) {
    addTimer(name, "stop")
    if (settings.printresults)
      printElapsed(name)
  }

  def search() {
    if (settings.dotiming) startTimer("getSearchDirs")
    val searchDirs = getSearchDirs(new File(settings.startpath))
    if (settings.dotiming) stopTimer("getSearchDirs")
    if (settings.verbose) {
      println("\nDirectories to be searched (%d):\n%s".format(searchDirs.size, searchDirs.mkString("\n")))
    }
    if (settings.dotiming) startTimer("getSearchFiles")
    val searchFiles = getSearchFiles(searchDirs)
    if (settings.dotiming) stopTimer("getSearchFiles")
    if (settings.verbose) {
      println("\nFiles to be searched (%d):\n%s".format(searchFiles.size, searchFiles.mkString("\n")))
    }
    if (settings.verbose) {
      println("\nStarting file search...\n")
    }
    if (settings.dotiming) startTimer("searchFiles")
    for (f <- searchFiles) {
      searchFile(f)
    }
    if (settings.dotiming) stopTimer("searchFiles")
    if (settings.verbose) {
      println("\nFile search complete.\n")
    }
    if (settings.printresults) {
      println("Search results (%d):".format(_searchResults.length))
      _searchResults.foreach(printSearchResult)
    }
    if (settings.listfiles) printFileList()
    if (settings.listlines) printLineList()
  }

  def searchFile(f: File) {
    if (FileUtil.isSearchableFile(f)) {
      if (FileUtil.isTextFile(f)) {
        searchTextFile(f)
      } else if (FileUtil.isBinaryFile(f)) {
        searchBinaryFile(f)
      }
    }
  }

  def searchTextFile(f: File) {
    if (settings.verbose) {
      println("Searching text file " + f.getPath)
    }
    if (settings.multilinesearch)
      searchTextFileContents(f)
    else
      searchTextFileLines(f)
  }

  def getLineCount(text: CharSequence) = {
    """(\r\n|\n)""".r.findAllIn(text).toList.length
  }

  def startOfLineIndexFromCurrent(text: CharSequence, currentIndex: Int): Int = {
    text.charAt(currentIndex) match {
      case '\n' => currentIndex
      case _ =>
        if (currentIndex > 0)
          startOfLineIndexFromCurrent(text, currentIndex-1)
        else 0
    }
  }

  def endOfLineIndexFromCurrent(text: CharSequence, currentIndex: Int): Int = {
    text.charAt(currentIndex) match {
      case '\n' => currentIndex
      case _ =>
        if (currentIndex < text.length)
          endOfLineIndexFromCurrent(text, currentIndex+1)
        else text.length
    }
  }

  def searchTextFileContents(f: File) {
    val source = Source.fromFile(f.getAbsolutePath)
    val contents = source.mkString
    var stop = false
    for (p <- settings.searchPatterns) {
      val matches = p.findAllIn(contents).matchData
      while (matches.hasNext && !stop) {
        val m = matches.next()
        val beforeText = m.before
        val beforeLineCount = 
          if (beforeText == null) 0
          else getLineCount(beforeText)
        val lineStartIndex = 
          if (beforeLineCount > 0)
            startOfLineIndexFromCurrent(contents, m.start)
          else 0
        val afterText = m.after
        val afterLineCount = 
          if (afterText == null) 0
          else getLineCount(afterText)
        val lineEndIndex = 
          if (afterLineCount > 0) endOfLineIndexFromCurrent(contents, m.start)
          else contents.length
        val line = contents.subSequence(lineStartIndex, lineEndIndex).toString
        val searchResult = new SearchResult(p, f, beforeLineCount+1, line)
        addSearchResult(searchResult)
        if (settings.firstmatch &&
            _fileMap.contains(f) &&
            _fileMap(f).exists(_.searchPattern == p))
          stop = true
      }
    }
  }

  def linesMatch(lines: Iterable[String], inPatterns: Set[Regex],
      outPatterns: Set[Regex]): Boolean = {
    (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns)) &&
    (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
  }

  def linesBeforeMatch(linesBefore: Iterable[String]): Boolean = {
    linesMatch(linesBefore, Set.empty[Regex] ++ settings.inLinesBeforePatterns,
      Set.empty[Regex] ++ settings.outLinesBeforePatterns)
  }

  def linesAfterMatch(linesAfter: Iterable[String]): Boolean = {
    linesMatch(linesAfter, Set.empty[Regex] ++ settings.inLinesAfterPatterns,
      Set.empty[Regex] ++ settings.outLinesAfterPatterns)
  }

  def searchTextFileLines(f: File) {
    var stop = false
    val source = Source.fromFile(f.getAbsolutePath)
    val lines = source.getLines()
    var lineNum: Int = 0
    val linesBefore = new mutable.ListBuffer[String]
    val linesAfter = new mutable.ListBuffer[String]
    while (lines.hasNext && !stop) {
      val line = 
        if (!linesAfter.isEmpty) linesAfter.remove(0)
        else lines.next()
      lineNum += 1
      if (settings.numlinesafter > 0) {
        while (linesAfter.length < settings.numlinesafter && lines.hasNext)
          linesAfter += lines.next
      }
      for (p <- settings.searchPatterns if p.findFirstIn(line) != None) {
        if ((linesBefore.isEmpty || linesBeforeMatch(linesBefore)) &&
            (linesAfter.isEmpty || linesAfterMatch(linesAfter))) {
          addSearchResult(new SearchResult(p, f, lineNum, line,
            linesBefore.toList, linesAfter.toList))
          if (settings.firstmatch &&
              _fileMap.contains(f) &&
              _fileMap(f).exists(_.searchPattern == p))
            stop = true
        }
      }
      if (settings.numlinesbefore > 0) {
        if (linesBefore.length == settings.numlinesbefore)
          linesBefore.remove(0, 1)
        if (linesBefore.length < settings.numlinesbefore)
          linesBefore += line
      }
    }
    source.close()
  }

  def searchBinaryFile(f: File) {
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

  def addSearchResult(r: SearchResult) {
    _searchResults.append(r)
    val resultListBuffer = _fileMap.getOrElse(r.file, mutable.ListBuffer.empty[SearchResult])
    resultListBuffer.append(r)
    _fileMap.put(r.file, resultListBuffer)
    //if (settings.printresults) printSearchResult(r)
  }

  def printSearchResult(r: SearchResult) {
    if (settings.searchPatterns.size > 1) {
      print("\"" + r.searchPattern + "\": ")
    }
    println(r)
  }

  def printFileList() {
    println("\nMatching files:")
    val files = _fileMap.keys.toList.sortWith(_.toString < _.toString)
    files.foreach(println(_))
  }

  def printLineList() {
    val lineset = mutable.Set[String]()
    for (r <- searchResults if r.line != null) {
      lineset.add(r.line.trim)
    }
    println("\nMatching lines:")
    val lines = lineset.toList.sortWith(_ < _)
    lines.foreach(println(_))
  }
}
