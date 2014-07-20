package scalasearch

import java.io.{InputStream, BufferedInputStream, File, FileInputStream}
import java.util.zip.{GZIPInputStream, ZipEntry, ZipFile}
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scala.collection.JavaConversions.enumerationAsScalaIterator
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

  private val _fileMap = mutable.Map[SearchFile, List[SearchResult]]()
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
    ((inPatterns.isEmpty || matchesAnyPattern(s, inPatterns))
     &&
     (outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns)))
  }

  def isSearchDir(d: File): Boolean = {
    isSearchDir(d.getName)
  }

  def isSearchDir(dirName: String): Boolean = {
    filterInByPatterns(dirName, settings.inDirPatterns, settings.outDirPatterns)
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

  def getSearchFileType(f: File): SearchFileType.SearchFileType = getSearchFileType(f.getName)

  def getSearchFileType(fileName: String): SearchFileType.SearchFileType = {
    if (isSearchFile(fileName)) SearchFileType.SearchFile
    else if (isArchiveSearchFile(fileName)) SearchFileType.ArchiveSearchFile
    else SearchFileType.NonSearchFile
  }

  def isSearchFile(f: File): Boolean = {
    isSearchFile(f.getName)
  }

  def isSearchFile(fileName: String): Boolean = {
    ((settings.inExtensions.isEmpty ||
      settings.inExtensions.contains(FileUtil.getExtension(fileName)))
      &&
      (settings.outExtensions.isEmpty ||
        !settings.outExtensions.contains(FileUtil.getExtension(fileName)))
      &&
      filterInByPatterns(fileName, settings.inFilePatterns,
        settings.outFilePatterns))
  }

  def isArchiveSearchFile(f: File): Boolean = {
    isArchiveSearchFile(f.getName)
  }

  def isArchiveSearchFile(fileName: String): Boolean = {
    settings.searchArchives &&
      filterInByPatterns(fileName, settings.inArchiveFilePatterns,
        settings.outArchiveFilePatterns)
  }

  def getSearchFilesForDirectory(dir:File): Iterable[SearchFile] = {
    val searchFiles = mutable.ArrayBuffer.empty[SearchFile]
    dir.listFiles().filterNot(_.isDirectory).map {
      f =>
        if (FileUtil.isArchiveFile(f) && isArchiveSearchFile(f)) {
          searchFiles += new SearchFile(f.getParent, f.getName)
        } else if (!settings.archivesOnly && isSearchFile(f)) {
          searchFiles += new SearchFile(f.getParent, f.getName)
        }
    }
    searchFiles.toList
  }

  def getSearchFiles(searchDirs:Iterable[File]): Iterable[SearchFile] = {
    def getFilteredFiles(dirs:Iterable[File]): Iterable[SearchFile] = {
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
    val searchFiles: Iterable[SearchFile] = getSearchFiles(searchDirs)
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

  def searchFile(sf: SearchFile) {
    searchFileSource(sf, Source.fromFile(sf.toFile))
  }

  def searchFileSource(sf: SearchFile, source: Source) {
    if (FileUtil.isSearchableFile(sf)) {
      if (FileUtil.isTextFile(sf)) {
        searchTextFileSource(sf, source)
      } else if (FileUtil.isBinaryFile(sf)) {
        searchBinaryFileSource(sf, source)
      } else if (FileUtil.isArchiveFile(sf)) {
        searchArchiveFileSource(sf, source)
      }
    }
  }

  def searchTextFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      println("Searching text file " + sf.getPathWithContainers)
    }
    if (settings.multiLineSearch)
      searchTextFileSourceContents(sf, source)
    else
      searchTextFileSourceLines(sf, source)
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

  def searchTextFileSourceContents(sf: SearchFile, source: Source) {
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
        val searchResult = new SearchResult(p, sf, beforeLineCount+1, line)
        addSearchResult(searchResult)
        if (settings.firstMatch &&
            _fileMap.contains(sf) &&
            _fileMap(sf).exists(_.searchPattern == p))
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

  def searchTextFileLines(sf: SearchFile) {
    searchTextFileSourceLines(sf, Source.fromFile(sf.toFile))
  }

  def searchTextFileSourceLines(f: SearchFile, source: Source) {
    var stop = false
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

  def searchBinaryFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      println("Searching binary file " + sf.toString)
    }
    val contents = source.mkString
    source.close()
    for (p <- settings.searchPatterns if p.findFirstIn(contents) != None) {
      addSearchResult(new SearchResult(p, sf, 0, null))
    }
  }

  def searchArchiveFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      println("Searching archive file " + sf.toString)
    }
    val zipExts = Set("zip", "jar", "war")
    if (zipExts contains FileUtil.getExtension(sf)) {
      searchZipFileSource(sf, source)
    } else if (FileUtil.getExtension(sf) == "gz" ||
      FileUtil.getExtension(sf) == "tgz") {
      searchGzFileSource(sf, source)
    } else if (FileUtil.getExtension(sf) == "bz2") {
      searchBz2FileSource(sf, source)
    } else {
      println("Currently unsupported archive file type: %s (%s)".
        format(FileUtil.getExtension(sf), sf.toString))
    }
  }

  def searchZipFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      println("Searching zip file " + sf.toString)
    }
    val zf = new ZipFile(sf.toFile)
    val entries = zf.entries().filterNot(_.isDirectory)
    val entryMap = mutable.LinkedHashMap.empty[String, List[ZipEntry]]
    entries foreach {
      ze =>
        val f = new File(ze.getName)
        entryMap(f.getParent) = entryMap.getOrElse(f.getParent,
          List.empty[ZipEntry]) :+ ze
    }
    entryMap foreach {
      e =>
        val dirName = e._1
        if (isSearchDir(dirName)) {
          val zes = e._2
          zes foreach {
            ze =>
              val fileName = new File(ze.getName).getName
              val searchFileType = getSearchFileType(fileName)
              if (searchFileType != SearchFileType.NonSearchFile) {
                val zsf = new SearchFile(sf.containers :+ sf.getPath, dirName, fileName)
                val zis = zf.getInputStream(zf.getEntry(zsf.getPath))
                searchFileType match {
                  case SearchFileType.SearchFile =>
                    searchFileSource(zsf, Source.fromInputStream(zis))
                  case SearchFileType.ArchiveSearchFile =>
                    searchArchiveFileSource(zsf, Source.fromInputStream(zis))
                  case _ => // do nothing
                }
                zis.close()
              }
          }
        }
    }
  }

  def searchTarFileInputStream(sf: SearchFile, is: InputStream) {
    if (settings.verbose) {
      println("Searching tar file " + sf.toString)
    }
    val tis = new TarArchiveInputStream(is)
    var entry: TarArchiveEntry = tis.getNextTarEntry
    while (entry != null) {
      if (!entry.isDirectory) {
        val dirName = new File(entry.getName).getParent
        if (isSearchDir(dirName)) {
          val fileName = new File(entry.getName).getName
          val searchFileType = getSearchFileType(fileName)
          if (searchFileType != SearchFileType.NonSearchFile) {
            var bytes = new Array[Byte](entry.getSize.toInt)
            val count = tis.read(bytes, 0, entry.getSize.toInt)
            if (count > 0) {
              val tzsf = new SearchFile(sf.containers, dirName, fileName)
              val source = Source.fromBytes(bytes)
              searchFileType match {
                case SearchFileType.SearchFile =>
                  searchFileSource(tzsf, source)
                case SearchFileType.ArchiveSearchFile =>
                  searchArchiveFileSource(tzsf, source)
                case _ => // do nothing
              }
            }
          }
        }
      }
      entry = tis.getNextTarEntry
    }
  }

  def searchGzFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      println("Searching gzip file " + sf.toString)
    }
    val containedFileName = sf.file.take(sf.file.length - 3)
    val gzsf = new SearchFile(sf.containers :+ sf.getPath, "", containedFileName)
    if (isSearchFile(gzsf.file) || FileUtil.getExtension(containedFileName) == "tar") {
      val gzis = new GZIPInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (FileUtil.getExtension(containedFileName) == "tar") {
        searchTarFileInputStream(gzsf, gzis)
      }
      else {
        val source = Source.fromInputStream(gzis)
        searchFileSource(gzsf, source)
      }
      gzis.close()
    }
  }

  def searchBz2FileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      println("Searching bzip2 file " + sf.toString)
    }
    val containedFileName = sf.file.take(sf.file.length - 4)
    val bzsf = new SearchFile(sf.containers :+ sf.getPath, "", containedFileName)
    if (isSearchFile(bzsf.file) || FileUtil.getExtension(containedFileName) == "tar") {
      val bzis = new BZip2CompressorInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (FileUtil.getExtension(containedFileName) == "tar") {
        searchTarFileInputStream(bzsf, bzis)
      }
      else {
        val source = Source.fromInputStream(bzis)
        searchFileSource(bzsf, source)
      }
      bzis.close()
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
    val dirs = _fileMap.keySet.map(_.path).toList.
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
