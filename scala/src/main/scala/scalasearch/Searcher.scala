package scalasearch

import java.io.{File, FileInputStream, InputStream}
import scala.collection.mutable
import scala.io.{BufferedSource, Source}
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

  private val _fileMap = mutable.Map[File, List[SearchResult]]()
  private val _searchResults = mutable.ListBuffer[SearchResult]()
  private val _timers = mutable.Map[String,Long]()

  def searchResults: List[SearchResult] = _searchResults.toList

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def anyMatchesAnyPattern(strings: Iterable[String], patterns: Set[Regex]):
    Boolean = {
    strings exists (matchesAnyPattern(_, patterns))
  }

  def filterInByPatterns(s:String, inPatterns:Set[Regex],
                         outPatterns:Set[Regex]): Boolean = {
    if (inPatterns.nonEmpty && !matchesAnyPattern(s, inPatterns)) {
      false
    } else if (outPatterns.nonEmpty && matchesAnyPattern(s, outPatterns)) {
      false
    } else {
      true
    }
  }

  def isSearchDir(d: File): Boolean = {
    filterInByPatterns(d.getName, settings.inDirPatterns, settings.outDirPatterns)
  }

  def getSearchDirs(startDir:File): Iterable[File] = {
    if (settings.verbose) println("getSearchDirs(%s)".format(startDir.toString))
    val searchDirs =
      if (isSearchDir(startDir)) Seq[File](startDir)
      else Seq.empty[File]
    def getFilteredDirs(dir:File): Iterable[File] = {
      val filteredDirs = dir.listFiles.filter(_.isDirectory).filter(isSearchDir)
      filteredDirs ++ filteredDirs.flatMap(getFilteredDirs)
    }
    searchDirs ++ getFilteredDirs(startDir)
  }

  def isSearchFile(f: File): Boolean = {
    if (settings.inExtensions.nonEmpty &&
      !settings.inExtensions.contains(FileUtil.getExtension(f))) {
      false
    } else if (settings.outExtensions.nonEmpty &&
      settings.outExtensions.contains(FileUtil.getExtension(f))) {
      false
    } else {
      filterInByPatterns(f.getName, settings.inFilePatterns, settings.outFilePatterns)
    }
  }

  def getSearchFilesForDirectory(dir:File): Iterable[File] = {
    dir.listFiles().filterNot(_.isDirectory).filter(isSearchFile)
  }

  def getSearchFiles(searchDirs:Iterable[File]): Iterable[File] = {
    def getFilteredFiles(dirs:Iterable[File]): Iterable[File] = {
      dirs.size match {
        case 0 => Nil
        case _ =>
          val filteredFiles = getSearchFilesForDirectory(dirs.head)
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
    if (settings.printResults)
      printElapsed(name)
  }

  def search() {
    if (settings.doTiming) startTimer("getSearchDirs")
    val searchDirs = getSearchDirs(new File(settings.startpath))
    if (settings.doTiming) stopTimer("getSearchDirs")
    if (settings.verbose) {
      println("\nDirectories to be searched (%d):\n%s".format(searchDirs.size,
        searchDirs.mkString("\n")))
    }
    if (settings.doTiming) startTimer("getSearchFiles")
    val searchFiles = getSearchFiles(searchDirs)
    if (settings.doTiming) stopTimer("getSearchFiles")
    if (settings.verbose) {
      println("\nFiles to be searched (%d):\n%s".format(searchFiles.size,
        searchFiles.mkString("\n")))
    }
    if (settings.verbose) {
      println("\nStarting file search...\n")
    }
    if (settings.doTiming) startTimer("searchFiles")
    for (f <- searchFiles) {
      searchFile(f)
    }
    if (settings.doTiming) stopTimer("searchFiles")
    if (settings.verbose) {
      println("\nFile search complete.\n")
    }
    if (settings.printResults) {
      println("Search results (%d):".format(_searchResults.length))
      _searchResults.foreach(printSearchResult)
    }
    if (settings.listDirs) printDirList()
    if (settings.listFiles) printFileList()
    if (settings.listLines) printLineList()
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
    if (settings.multiLineSearch)
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
        if (settings.firstMatch &&
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
    searchTextFileInputStreamLines(f, new FileInputStream(f))
  }

  def searchTextFileInputStreamLines(f: File, is: InputStream) {
    var stop = false
    val source = Source.fromInputStream(is)
    val lines = source.getLines()
    var lineNum: Int = 0
    val linesBefore = new mutable.ListBuffer[String]
    val linesAfter = new mutable.ListBuffer[String]
    while (lines.hasNext && !stop) {
      val line = 
        if (linesAfter.nonEmpty) linesAfter.remove(0)
        else lines.next()
      lineNum += 1
      if (settings.linesAfter > 0) {
        while (linesAfter.length < settings.linesAfter && lines.hasNext)
          linesAfter += lines.next
      }
      // search the line with each searchPatterns
      for (p <- settings.searchPatterns if p.findFirstIn(line).isDefined) {
        if ((linesBefore.isEmpty || linesBeforeMatch(linesBefore)) &&
            (linesAfter.isEmpty  || linesAfterMatch(linesAfter))) {

          // take care of linesAfterToPatterns or linesAfterUntilPatterns
          var lineAfterToMatch = false
          var lineAfterUntilMatch = false
          if (settings.linesAfterToPatterns.nonEmpty ||
            settings.linesAfterUntilPatterns.nonEmpty) {
            // check to see if linesAfter has a match
            if (settings.linesAfterToPatterns.nonEmpty &&
              anyMatchesAnyPattern(linesAfter, settings.linesAfterToPatterns)) {
              lineAfterToMatch = true
            } else if (settings.linesAfterUntilPatterns.nonEmpty &&
              anyMatchesAnyPattern(linesAfter, settings.linesAfterUntilPatterns)) {
              lineAfterUntilMatch = true
            }
            // if not read more lines into linesAfter until match or EOF
            while (lines.hasNext && !lineAfterToMatch && !lineAfterUntilMatch) {
              val nextLine = lines.next()
              linesAfter += nextLine
              if (settings.linesAfterToPatterns.nonEmpty &&
                matchesAnyPattern(nextLine, settings.linesAfterToPatterns)) {
                lineAfterToMatch = true
              } else if (settings.linesAfterUntilPatterns.nonEmpty &&
                matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns)) {
                lineAfterUntilMatch = true
              }
            }
          }
          val resLinesAfter =
            if (lineAfterUntilMatch) linesAfter.init.toList
            else linesAfter.toList

          // add the search result
          addSearchResult(new SearchResult(p, f, lineNum, line,
            linesBefore.toList, resLinesAfter))
          if (settings.firstMatch &&
              _fileMap.contains(f) &&
              _fileMap(f).exists(_.searchPattern == p))
            stop = true
        }
      }
      if (settings.linesBefore > 0) {
        if (linesBefore.length == settings.linesBefore)
          linesBefore.remove(0, 1)
        if (linesBefore.length < settings.linesBefore)
          linesBefore += line
      }
    }
    source.close()
  }

  def searchBinaryFile(f: File) {
    if (settings.verbose) {
      println("Searching binary file " + f.getPath)
    }
    val source: BufferedSource = Source.fromFile(f.getAbsolutePath)
    val contents = source.mkString
    source.close()
    for (p <- settings.searchPatterns if p.findFirstIn(contents) != None) {
      addSearchResult(new SearchResult(p, f, 0, null))
    }
  }

  def addSearchResult(r: SearchResult) {
    _searchResults.append(r)
    _fileMap.put(r.file, _fileMap.getOrElse(r.file, List.empty[SearchResult]) :+ r )
  }

  def printSearchResult(r: SearchResult) {
    if (settings.searchPatterns.size > 1) {
      print("\"" + r.searchPattern + "\": ")
    }
    println(r)
  }

  def printDirList() {
    val dirs = _fileMap.keySet.map(_.getParentFile).toList.
      sortWith(_.toString < _.toString)
    println("\nMatching directories (%d directories):".format(dirs.length))
    dirs.foreach(println(_))
  }

  def printFileList() {
    val files = _fileMap.keys.toList.sortWith(_.toString < _.toString)
    println("\nMatching files (%d files):".format(files.length))
    files.foreach(println(_))
  }

  def printLineList() {
    val lineset = mutable.Set[String]()
    for (r <- searchResults if r.line != null) {
      lineset.add(r.line.trim)
    }
    val lines = lineset.toList.sortWith(_ < _)
    println("\nMatching lines (%d unique lines):".format(lines.length))
    lines.foreach(println(_))
  }
}
