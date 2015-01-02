package scalasearch

import java.io.{InputStream, BufferedInputStream, File, FileInputStream}
import java.util.zip.{GZIPInputStream, ZipEntry, ZipFile}
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

class Searcher (settings: SearchSettings) {
  def validateSettings() {
    assert(settings.startpath.nonEmpty, "Missing startpath")
    assert(settings.searchPatterns.size > 0, "No search patterns defined")
  }
  validateSettings()

  if (settings.debug) {
    log("settings: "+settings)
  }

  private val _fileMap = mutable.Map[SearchFile, List[SearchResult]]()
  private val _searchResults = mutable.ListBuffer[SearchResult]()
  private val _timers = mutable.Map[String,Long]()

  def searchResults: List[SearchResult] = _searchResults.toList

  def log(message: String): Unit = {
    println(message)
  }

  def matchesAnyPattern(s: String, patterns: Iterable[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def anyMatchesAnyPattern(strings: Iterable[String], patterns: Iterable[Regex]):
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

  def pathElemsFromPath(path: String): Iterable[String] = {
    path.split(File.separator).filterNot(p => Set(".", "..").contains(p))
  }

  def isSearchDir(dirName: String): Boolean = {
    val pathElems = pathElemsFromPath(dirName)
    if (pathElems.exists(_.startsWith(".") && settings.excludeHidden))
      false
    else
      filterInByPatterns(dirName, settings.inDirPatterns, settings.outDirPatterns)
  }

  private def getSearchSubDirs(dir:File): Iterable[File] = {
    val searchSubDirs = dir.listFiles.filter(_.isDirectory).filter(isSearchDir)
    searchSubDirs ++ searchSubDirs.flatMap(getSearchSubDirs)
  }

  def getSearchDirs(startDir:File): Iterable[File] = {
    if (settings.verbose) log("getSearchDirs(%s)".format(startDir.toString))
    val startDirs =
      if (isSearchDir(startDir)) Seq[File](startDir)
      else Seq.empty[File]
    val subDirs =
      if (settings.recursive) getSearchSubDirs(startDir)
      else Seq.empty[File]
    startDirs ++ subDirs
  }


  def isSearchFile(f: File): Boolean = {
    isSearchFile(f.getName)
  }

  def isSearchFile(fileName: String): Boolean = {
    if (settings.excludeHidden && FileUtil.isHiddenFile(fileName)) false
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
    if (settings.excludeHidden && FileUtil.isHiddenFile(fileName)) false
    ((settings.inArchiveExtensions.isEmpty ||
      settings.inArchiveExtensions.contains(FileUtil.getExtension(fileName)))
      &&
      (settings.outArchiveExtensions.isEmpty ||
        !settings.outArchiveExtensions.contains(FileUtil.getExtension(fileName)))
      &&
      filterInByPatterns(fileName, settings.inArchiveFilePatterns,
        settings.outArchiveFilePatterns))
  }

  def getSearchFilesForDirectory(dir:File): Iterable[SearchFile] = {
    val searchFiles: Array[Option[SearchFile]] = dir.listFiles().filterNot(_.isDirectory).map {
      f =>
        FileUtil.getFileType(f) match {
          case FileType.Unknown => None
          case fileType@FileType.Archive =>
            if (settings.searchArchives && isArchiveSearchFile(f))
              Some(new SearchFile(f.getParent, f.getName, fileType))
            else
              None
          case fileType =>
            if (!settings.archivesOnly && isSearchFile(f))
              Some(new SearchFile(f.getParent, f.getName, fileType))
            else
              None
        }
    }
    // this cast is required regardless of what IDEA says
    searchFiles.flatten.asInstanceOf[Array[SearchFile]]
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

  private def addTimer(name:String, action:String) {
    _timers.put(name+":"+action, System.currentTimeMillis)
  }

  private def startTimer(name:String) {
    addTimer(name, "start")
  }

  private def getElapsed(name:String): Long = {
    val startTime = _timers(name+":start")
    val stopTime = _timers(name+":stop")
    stopTime - startTime
  }

  def printElapsed(name:String) {
    val elapsed = getElapsed(name)
    log("Elapsed time for \"%s\": %d milliseconds".format(name, elapsed))
  }

  private def stopTimer(name:String) {
    addTimer(name, "stop")
    if (settings.printResults)
      printElapsed(name)
  }

  def search() {
    if (settings.doTiming) startTimer("getSearchDirs")
    val searchDirs = getSearchDirs(new File(settings.startpath))
    if (settings.doTiming) stopTimer("getSearchDirs")
    if (settings.verbose) {
      log("\nDirectories to be searched (%d):\n%s".format(searchDirs.size,
        searchDirs.mkString("\n")))
    }
    if (settings.doTiming) startTimer("getSearchFiles")
    val searchFiles: Iterable[SearchFile] = getSearchFiles(searchDirs)
    if (settings.doTiming) stopTimer("getSearchFiles")
    if (settings.verbose) {
      log("\nFiles to be searched (%d):\n%s".format(searchFiles.size,
        searchFiles.mkString("\n")))
    }
    if (settings.verbose) {
      log("\nStarting file search...\n")
    }
    if (settings.doTiming) startTimer("searchFiles")
    for (f <- searchFiles) {
      searchFile(f)
    }
    if (settings.doTiming) stopTimer("searchFiles")
    if (settings.verbose) {
      log("\nFile search complete.\n")
    }
  }

  def searchFile(sf: SearchFile) {
    searchFileSource(sf, Source.fromFile(sf.toFile))
  }

  def searchFileSource(sf: SearchFile, source: Source) {
    FileUtil.getFileType(sf) match {
      case FileType.Text =>
        searchTextFileSource(sf, source)
      case FileType.Binary =>
        searchBinaryFileSource(sf, source)
      case FileType.Archive =>
        searchArchiveFileSource(sf, source)
      case _ =>
        log("Skipping unknown file type: " + sf)
    }
  }

  def searchTextFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      log("Searching text file " + sf.getPathWithContainers)
    }
    if (settings.multiLineSearch)
      searchTextFileSourceContents(sf, source)
    else
      searchTextFileSourceLines(sf, source)
  }

  def searchTextFileSourceContents(sf: SearchFile, source: Source) {
    val contents = source.mkString
    searchMultiLineString(contents).foreach { r =>
      addSearchResult(new SearchResult(r.searchPattern, sf, r.lineNum,
        r.matchStartIndex, r.matchEndIndex, r.line, r.linesBefore, r.linesAfter))
    }
  }

  def searchMultiLineString(s: String): Seq[StringSearchResult] = {
    val stringSearchResults = mutable.ArrayBuffer.empty[StringSearchResult]
    for (p <- settings.searchPatterns) {
      stringSearchResults ++= searchMultiLineStringForPattern(s, p)
    }
    stringSearchResults
  }

  def getLinesAfterFromMultiLineString(s: String, startIndex: Int,
                                       lineIndices: Seq[(Int, Int)]): List[String] = {
    if (settings.hasLinesAfterToOrUntilPatterns) {
      val matchIndices = (settings.linesAfterToPatterns ++ settings.linesAfterUntilPatterns).map {
        p =>
          p.findFirstMatchIn(s.substring(startIndex)) match {
            case Some(m) => m.start + startIndex
            case None => -1
          }
      }.filter(_ > -1)
      if (matchIndices.nonEmpty) {
        val lines = lineIndices.
          filter(_._1 < matchIndices.min).
          map(li => s.substring(li._1, li._2)).toList
        if (settings.hasLinesAfterUntilPatterns && lines.nonEmpty)
          lines.init
        else
          lines
      } else {
        List.empty[String]
      }
    } else if (settings.linesAfter > 0) {
      lineIndices.
        take(settings.linesAfter).
        map(li => s.substring(li._1, li._2)).toList
    } else {
      List.empty[String]
    }
  }

  def searchMultiLineStringForPattern(s: String, p: Regex): Seq[StringSearchResult] = {
    val lineIndices: Seq[(Int, Int)] = Searcher.getLineIndices(s)
    val stringSearchResults = mutable.ArrayBuffer.empty[StringSearchResult]
    val matches = p.findAllIn(s).matchData
    var stop = false
    while (matches.hasNext && !stop) {
      val m = matches.next()
      val thisLineIndices = lineIndices.filter(_._1 <= m.start).last
      val beforeLineCount = lineIndices.count(_._1 < thisLineIndices._1)
      val line = s.substring(thisLineIndices._1, thisLineIndices._2)
      val linesBefore: List[String] =
        if (settings.linesBefore > 0) {
          lineIndices.filter(_._1 < thisLineIndices._1).
            takeRight(settings.linesBefore).
            map(li => s.substring(li._1, li._2)).toList
        } else {
          List.empty[String]
        }
      val linesAfter = getLinesAfterFromMultiLineString(s, thisLineIndices._2,
        lineIndices.filter(_._1 > thisLineIndices._2))
      if (linesBeforeMatch(linesBefore) && linesAfterMatch(linesAfter)) {
        stringSearchResults += new StringSearchResult(
          p,
          beforeLineCount + 1,
          m.start - thisLineIndices._1 + 1,
          m.end - thisLineIndices._1 + 1,
          line,
          linesBefore,
          linesAfter)
        if (settings.firstMatch)
          stop = true
      }
    }
    stringSearchResults
  }

  private def linesMatch(lines: Iterable[String], inPatterns: Iterable[Regex],
      outPatterns: Iterable[Regex]): Boolean = {
    (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns)) &&
    (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
  }

  private def linesBeforeMatch(linesBefore: Iterable[String]): Boolean = {
    if (settings.hasLinesBeforePatterns) {
      linesMatch(linesBefore, settings.inLinesBeforePatterns,
        settings.outLinesBeforePatterns)
    } else true
  }

  private def linesAfterMatch(linesAfter: Iterable[String]): Boolean = {
    if (settings.hasLinesAfterToPatterns) {
      linesAfter.nonEmpty &&
        anyMatchesAnyPattern(linesAfter, settings.linesAfterToPatterns)
    } else if (settings.hasLinesAfterUntilPatterns) {
      linesAfter.nonEmpty
    } else if (settings.hasLinesAfterPatterns) {
      linesMatch(linesAfter, settings.inLinesAfterPatterns,
        settings.outLinesAfterPatterns)
    } else true
  }

  def searchTextFileLines(sf: SearchFile) {
    searchTextFileSourceLines(sf, Source.fromFile(sf.toFile))
  }

  def searchTextFileSourceLines(sf: SearchFile, source: Source) {
    searchLineStringIterator(source.getLines()).foreach { r =>
      addSearchResult(new SearchResult(r.searchPattern, sf, r.lineNum,
        r.matchStartIndex, r.matchEndIndex, r.line, r.linesBefore, r.linesAfter))
    }
  }

  def searchLineStringIterator(lines: Iterator[String]): Seq[StringSearchResult] = {
    var stop = false
    var lineNum: Int = 0
    val linesBefore = new mutable.ListBuffer[String]
    val linesAfter = new mutable.ListBuffer[String]
    val patternMatches = mutable.Map.empty[Regex,Int]
    val stringSearchResults = mutable.ArrayBuffer.empty[StringSearchResult]
    // TODO: verify that this works when matches are on last line
    while (lines.hasNext && !stop) {
      lineNum += 1
      val line =
        if (linesAfter.nonEmpty) linesAfter.remove(0)
        else lines.next()
      if (settings.linesAfter > 0) {
        while (linesAfter.length < settings.linesAfter && lines.hasNext)
          linesAfter += lines.next
      }

      // make sure linesBefore and linesAfter match before continuing
      if ((settings.linesBefore == 0 || linesBefore.isEmpty || linesBeforeMatch(linesBefore)) &&
          (settings.linesAfter == 0  || linesAfter.isEmpty  || linesAfterMatch(linesAfter))) {

        // search the line with each searchPatterns
        for (p <- settings.searchPatterns) {
          for (m <- p.findAllIn(line).matchData) {
            // take care of linesAfterToPatterns or linesAfterUntilPatterns
            var linesAfterToMatch = false
            var linesAfterUntilMatch = false
            if (settings.hasLinesAfterToOrUntilPatterns) {
              // check to see if linesAfter has a match
              if (settings.linesAfterToPatterns.nonEmpty &&
                anyMatchesAnyPattern(linesAfter, settings.linesAfterToPatterns)) {
                linesAfterToMatch = true
              } else if (settings.linesAfterUntilPatterns.nonEmpty &&
                anyMatchesAnyPattern(linesAfter, settings.linesAfterUntilPatterns)) {
                linesAfterUntilMatch = true
              }
              // if not read more lines into linesAfter until match or EOF
              while (lines.hasNext && !linesAfterToMatch && !linesAfterUntilMatch) {
                val nextLine = lines.next()
                linesAfter += nextLine
                if (settings.linesAfterToPatterns.nonEmpty &&
                  matchesAnyPattern(nextLine, settings.linesAfterToPatterns)) {
                  linesAfterToMatch = true
                } else if (settings.linesAfterUntilPatterns.nonEmpty &&
                  matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns)) {
                  linesAfterUntilMatch = true
                }
              }
            }
            val resLinesAfter =
              if (linesAfterUntilMatch) linesAfter.init.toList
              else linesAfter.toList

            if (settings.firstMatch && patternMatches.contains(p)) {
              stop = true
            } else if (!settings.hasLinesAfterToOrUntilPatterns ||
                (settings.hasLinesAfterToOrUntilPatterns &&
                  (linesAfterToMatch || linesAfterUntilMatch))) {
              stringSearchResults += new StringSearchResult(
                p,
                lineNum,
                m.start + 1,
                m.end + 1,
                line,
                linesBefore.toList,
                resLinesAfter)
              patternMatches(p) = 1
            }
          }
        }
      }
      if (settings.linesBefore > 0) {
        if (linesBefore.length == settings.linesBefore)
          linesBefore.remove(0, 1)
        if (linesBefore.length < settings.linesBefore)
          linesBefore += line
      }
    }
    stringSearchResults
  }

  def searchBinaryFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      log("Searching binary file " + sf.toString)
    }
    val contents = source.mkString
    source.close()
    for (p <- settings.searchPatterns if p.findFirstIn(contents) != None) {
      addSearchResult(new SearchResult(p, sf, 0, null))
    }
  }

  def searchArchiveFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      log("Searching archive file " + sf.toString)
    }
    if (FileUtil.isZipArchiveFile(sf))
      searchZipFileSource(sf, source)
    else if (FileUtil.isGzArchiveFile(sf))
      searchGzFileSource(sf, source)
    else if (FileUtil.isBz2ArchiveFile(sf))
      searchBz2FileSource(sf, source)
    else if (FileUtil.isTarArchiveFile(sf)) {
      val tis = new BufferedInputStream(new FileInputStream(sf.getPath))
      searchTarFileInputStream(sf, tis)
    } else {
      log("Currently unsupported archive file type: %s (%s)".
        format(FileUtil.getExtension(sf), sf.toString))
    }
  }

  def searchZipFileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      log("Searching zip file " + sf.toString)
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
    entryMap foreach { e =>
      val dirName = e._1
      if (isSearchDir(dirName)) {
        val zes = e._2
        zes foreach { ze =>
          val file = new File(ze.getName)
          val fileType = FileUtil.getFileType(file)
          if (fileType != FileType.Unknown) {
            val zsf = new SearchFile(sf.containers :+ sf.getPath, dirName,
              file.getName, fileType)
            val zis = zf.getInputStream(zf.getEntry(zsf.getPath))
            fileType match {
              case FileType.Archive =>
                searchArchiveFileSource(zsf, Source.fromInputStream(zis))
              case _ =>
                searchFileSource(zsf, Source.fromInputStream(zis))
            }
            zis.close()
          }
        }
      }
    }
  }

  def searchTarFileInputStream(sf: SearchFile, is: InputStream) {
    if (settings.verbose) {
      log("Searching tar file " + sf.toString)
    }
    val tis = new TarArchiveInputStream(is)
    var entry: TarArchiveEntry = tis.getNextTarEntry
    while (entry != null) {
      if (!entry.isDirectory) {
        val dirName = new File(entry.getName).getParent
        if (isSearchDir(dirName)) {
          val file = new File(entry.getName)
          val fileType = FileUtil.getFileType(file)
          if (fileType != FileType.Unknown) {
            var bytes = new Array[Byte](entry.getSize.toInt)
            val count = tis.read(bytes, 0, entry.getSize.toInt)
            if (count > 0) {
              val tzsf = new SearchFile(sf.containers, dirName, file.getName,
                fileType)
              val source = Source.fromBytes(bytes)
              fileType match {
                case FileType.Archive =>
                  searchArchiveFileSource(tzsf, source)
                case _ =>
                  searchFileSource(tzsf, source)
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
      log("Searching gzip file " + sf.toString)
    }
    val containedFileName = sf.fileName.split("\\.").init.mkString(".")
    val containedFileType = FileUtil.getFileType(sf)
    val containedExtension = FileUtil.getExtension(sf)
    val gzsf = new SearchFile(sf.containers :+ sf.getPath, "", containedFileName,
      containedFileType)
    if (containedExtension == "tar" || isSearchFile(gzsf.fileName)) {
      val gzis = new GZIPInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == "tar") {
        searchTarFileInputStream(gzsf, gzis)
      } else {
        val source = Source.fromInputStream(gzis)
        searchFileSource(gzsf, source)
      }
      gzis.close()
    }
  }

  def searchBz2FileSource(sf: SearchFile, source: Source) {
    if (settings.verbose) {
      log("Searching bzip2 file " + sf.toString)
    }
    val containedFileName = sf.fileName.split("\\.").init.mkString(".")
    val containedFileType = FileUtil.getFileType(sf)
    val containedExtension = FileUtil.getExtension(sf)
    val bzsf = new SearchFile(sf.containers :+ sf.getPath, "", containedFileName,
      containedFileType)
    if (containedExtension == "tar" || isSearchFile(bzsf.fileName)) {
      val bzis = new BZip2CompressorInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == "tar") {
        searchTarFileInputStream(bzsf, bzis)
      }
      else {
        val source = Source.fromInputStream(bzis)
        searchFileSource(bzsf, source)
      }
      bzis.close()
    }
  }

  private def addSearchResult(r: SearchResult) {
    _searchResults.append(r)
    _fileMap.put(r.file, _fileMap.getOrElse(r.file, List.empty[SearchResult]) :+ r)
  }

  def printSearchResults() {
    _searchResults.foreach(printSearchResult)
 }

  def printSearchResult(r: SearchResult) {
    if (settings.searchPatterns.size > 1) {
      print("\"" + r.searchPattern + "\": ")
    }
    log(r.toString)
  }

  def getMatchingDirs:List[File] = {
    _fileMap.keySet.map(_.toFile).map(_.getParentFile).toList.
      sortWith(_.toString < _.toString)
  }

  def printMatchingDirs() {
    val dirs = getMatchingDirs
    log("\nMatching directories (%d directories):".format(dirs.length))
    dirs.foreach(f => log(f.toString))
  }

  def getMatchingFiles: List[File] = {
    _fileMap.keySet.map(_.toFile).toList.sortWith(_.toString < _.toString)
  }

  def printMatchingFiles() {
    val files = getMatchingFiles
    log("\nMatching files (%d files):".format(files.length))
    files.foreach(f => log(f.toString))
  }

  def getMatchingLines:List[String] = {
    val allLines = mutable.ArrayBuffer[String]()
    for (r <- searchResults if r.line != null) {
      allLines.append(r.line.trim)
    }
    val lines =
      if (settings.uniqueLines) allLines.toSet.toList.sortWith(_ < _)
      else allLines.toList.sortWith(_ < _)
    lines
  }

  def printMatchingLines() {
    val lines = getMatchingLines
    val hdr =
      if (settings.uniqueLines)
        "\nMatching lines (%d unique lines)".format(lines.length)
      else
        "\nMatching lines (%d lines)".format(lines.length)
    log(hdr)
    lines.foreach(log)
  }
}

// doing this so I can test methods in Searcher class that can be static
object Searcher {
  def getLineIndices(contents:String): Seq[(Int,Int)] = {
    val newLineIndices = contents.zipWithIndex.collect { case ('\n',i) => i }
    if (newLineIndices.length > 0) {
      val lineIndices = mutable.ArrayBuffer[(Int,Int)]((0,newLineIndices(0)))
      lineIndices ++= newLineIndices.map(_ + 1).zip(newLineIndices.tail :+ contents.length)
      lineIndices
    } else {
      Seq[(Int,Int)]((0,contents.length-1))
    }
  }
}