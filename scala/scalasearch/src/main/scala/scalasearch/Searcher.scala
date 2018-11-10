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
  def validateSettings(): Unit = {
    settings.startPath match {
      case Some(path) if path.length > 0 =>
        if (!new File(path).exists()) {
          throw new SearchException("Startpath not found")
        }
      case _ =>
        throw new SearchException("Startpath not defined")
    }
    if (settings.searchPatterns.isEmpty) {
      throw new SearchException("No search patterns defined")
    }
  }
  validateSettings()

  private val currentPath = "."
  private val tarExtension = "tar"

  private val _searchResults = mutable.ListBuffer[SearchResult]()

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def anyMatchesAnyPattern(strings: Seq[String], patterns: Set[Regex]):
    Boolean = {
    strings exists (matchesAnyPattern(_, patterns))
  }

  def filterByPatterns(s: String, inPatterns: Set[Regex], outPatterns: Set[Regex]):
    Boolean = {
    ((inPatterns.isEmpty || matchesAnyPattern(s, inPatterns))
     &&
     (outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns)))
  }

  def isSearchDir(d: File): Boolean = {
    isSearchDir(d.getName)
  }

  def isSearchDir(dirName: String): Boolean = {
    val pathElems = FileUtil.splitPath(dirName)
    if (pathElems.exists(p => FileUtil.isHidden(p)) && settings.excludeHidden) {
      false
    } else {
      filterByPatterns(dirName, settings.inDirPatterns, settings.outDirPatterns)
    }
  }

  private def getSearchSubDirs(dir: File): Seq[File] = {
    val searchSubDirs = dir.listFiles.filter(_.isDirectory).filter(isSearchDir)
    searchSubDirs ++ searchSubDirs.flatMap(getSearchSubDirs)
  }

  def getSearchDirs(startDir: File): Seq[File] = {
    if (settings.verbose) {
      Common.log("getSearchDirs(%s)".format(startDir.toString))
    }
    val startDirs =
      if (isSearchDir(startDir)) {
        Seq[File](startDir)
      } else {
        Seq.empty[File]
      }
    val subDirs =
      if (settings.recursive) {
        getSearchSubDirs(startDir)
      } else {
        Seq.empty[File]
      }
    startDirs ++ subDirs
  }

  def isSearchFile(f: File): Boolean = {
    isSearchFile(f.getName)
  }

  def isSearchFile(fileName: String): Boolean = {
    if (FileUtil.isHidden(fileName) && settings.excludeHidden) {
      false
    } else {
      val ext = FileUtil.getExtension(fileName)
      ((settings.inExtensions.isEmpty ||
        settings.inExtensions.contains(ext))
        &&
        (settings.outExtensions.isEmpty ||
          !settings.outExtensions.contains(ext))
        &&
        filterByPatterns(fileName, settings.inFilePatterns,
          settings.outFilePatterns))
    }
  }

  def isArchiveSearchFile(f: File): Boolean = {
    isArchiveSearchFile(f.getName)
  }

  def isArchiveSearchFile(fileName: String): Boolean = {
    if (FileUtil.isHidden(fileName) && settings.excludeHidden) {
      false
    } else {
      val ext = FileUtil.getExtension(fileName)
      ((settings.inArchiveExtensions.isEmpty ||
        settings.inArchiveExtensions.contains(ext))
        &&
        (settings.outArchiveExtensions.isEmpty ||
          !settings.outArchiveExtensions.contains(ext))
        &&
        filterByPatterns(fileName, settings.inArchiveFilePatterns,
          settings.outArchiveFilePatterns))
    }
  }

  def filterFile(f: File): Boolean = {
    FileTypes.getFileType(f) match {
      case FileType.Unknown => false
      case fileType@FileType.Archive =>
        settings.searchArchives && isArchiveSearchFile(f)
      case _ =>
        !settings.archivesOnly && isSearchFile(f)
    }
  }

  def getSearchFilesForDirectory(dir: File): Seq[SearchFile] = {
    dir.listFiles().filterNot(_.isDirectory).filter(filterFile).map { f =>
        new SearchFile(f.getParent, f.getName, FileTypes.getFileType(f))
    }
  }

  def getSearchFiles(searchDirs: Seq[File]): Seq[SearchFile] = {
    def getFilteredFiles(dirs: Seq[File]): Seq[SearchFile] = {
      dirs.size match {
        case 0 => Nil
        case _ =>
          getSearchFilesForDirectory(dirs.head) ++ getFilteredFiles(dirs.tail)
      }
    }
    getFilteredFiles(searchDirs)
  }

  def listToString(stringList: Iterable[Any]): String = {
    stringList.mkString("[\"", "\", \"", "\"]")
  }

  def search(): Unit = {
    val startPathFile = new File(settings.startPath.get)
    if (startPathFile.isDirectory) {
      if (isSearchDir(startPathFile)) {
        searchPath(startPathFile)
      } else {
        throw new SearchException("Startpath does not match search settings")
      }
    } else if (startPathFile.isFile) {
      if (filterFile(startPathFile)) {
        val fileType = FileTypes.getFileType(startPathFile)
        var d: File = startPathFile.getParentFile
        if (null == d) d = new File(currentPath)
        searchFile(new SearchFile(d.getPath, startPathFile.getName, fileType))
      } else {
        throw new SearchException("Startpath does not match search settings")
      }
    }
  }

  def searchPath(filePath: File): Unit = {
    val searchDirs = getSearchDirs(new File(settings.startPath.get))
    if (settings.verbose) {
      Common.log("\nDirectories to be searched (%d):\n%s".format(searchDirs.size,
        searchDirs.mkString("\n")))
    }
    val files: Iterable[SearchFile] = getSearchFiles(searchDirs)
    if (settings.verbose) {
      Common.log("\nFiles to be searched (%d):\n%s".format(files.size,
        files.mkString("\n")))
    }
    if (settings.verbose) {
      Common.log("\nStarting file search...\n")
    }
    searchFiles(files)
    if (settings.verbose) {
      Common.log("\nFile search complete.\n")
    }
  }

  def searchFiles(files: Traversable[SearchFile]): Unit = {
    for (f <- files) {
      searchFile(f)
    }
  }

  def searchFile(sf: SearchFile): Unit = {
    FileTypes.getFileType(sf) match {
      case FileType.Text =>
        // TODO: some very basic encoding detection, for now using arbitrary
        // single-byte encoding
        searchTextFileSource(sf, Source.fromFile(sf.toFile, "ISO-8859-1"))
      case FileType.Binary =>
        searchBinaryFileSource(sf, Source.fromFile(sf.toFile, "ISO-8859-1"))
      case FileType.Archive =>
        searchArchiveFileSource(sf, Source.fromFile(sf.toFile))
      case _ =>
        Common.log("Skipping unknown file type: %s".format(sf))
    }
  }

  private def searchFileSource(sf: SearchFile, source: Source): Unit = {
    FileTypes.getFileType(sf) match {
      case FileType.Text =>
        searchTextFileSource(sf, source)
      case FileType.Binary =>
        searchBinaryFileSource(sf, source)
      case FileType.Archive =>
        searchArchiveFileSource(sf, source)
      case _ =>
        Common.log("Skipping unknown file type: %s".format(sf))
    }
  }

  private def searchTextFileSource(sf: SearchFile, source: Source): Unit = {
    if (settings.verbose) {
      Common.log("Searching text file %s".format(sf.getPathWithContainers))
    }
    if (settings.multiLineSearch) {
      searchTextFileSourceContents(sf, source)
    } else {
      searchTextFileSourceLines(sf, source)
    }
  }

  private def searchTextFileSourceContents(sf: SearchFile, source: Source): Unit = {
    val contents = source.mkString
    searchMultiLineString(contents).foreach { r =>
      addSearchResult(r.copy(file=Some(sf)))
    }
  }

  def searchMultiLineString(s: String): Seq[SearchResult] = {
    val results = mutable.ArrayBuffer.empty[SearchResult]
    settings.searchPatterns.foreach { p =>
      results ++= searchMultiLineStringForPattern(s, p)
    }
    results
  }

  private def getLinesAfterFromMultiLineString(s: String, startIndex: Int,
                                               lineIndices: Seq[(Int, Int)]): Seq[String] = {
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
          map(li => s.substring(li._1, li._2))
        if (settings.hasLinesAfterUntilPatterns && lines.nonEmpty) {
          lines.init
        } else {
          lines
        }
      } else {
        Seq.empty[String]
      }
    } else if (settings.linesAfter > 0) {
      lineIndices.
        take(settings.linesAfter).
        map(li => s.substring(li._1, li._2))
    } else {
      Seq.empty[String]
    }
  }

  private def searchMultiLineStringForPattern(s: String, p: Regex): Seq[SearchResult] = {
    val lineIndices: Seq[(Int, Int)] = Searcher.getLineIndices(s)
    val results = mutable.ArrayBuffer.empty[SearchResult]
    val matches = p.findAllIn(s).matchData
    var stop = false
    while (matches.hasNext && !stop) {
      val m = matches.next()
      val thisLineIndices: (Int, Int) = lineIndices.filter(_._1 <= m.start).last
      val beforeLineCount = lineIndices.count(_._1 < thisLineIndices._1)
      val line = s.substring(thisLineIndices._1, thisLineIndices._2)
      val linesBefore =
        if (settings.linesBefore > 0) {
          lineIndices.filter(_._1 < thisLineIndices._1).
            takeRight(settings.linesBefore).
            map(li => s.substring(li._1, li._2))
        } else {
          Seq.empty[String]
        }
      val linesAfter = getLinesAfterFromMultiLineString(s, thisLineIndices._2,
        lineIndices.filter(_._1 > thisLineIndices._2))
      if (linesBeforeMatch(linesBefore) && linesAfterMatch(linesAfter)) {
        results += new SearchResult(
          p,
          None,
          beforeLineCount + 1,
          m.start - thisLineIndices._1 + 1,
          m.end - thisLineIndices._1 + 1,
          line,
          linesBefore,
          linesAfter)
        if (settings.firstMatch) {
          stop = true
        }
      }
    }
    results
  }

  private def linesMatch(lines: Seq[String], inPatterns: Set[Regex],
      outPatterns: Set[Regex]): Boolean = {
    (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns)) &&
    (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
  }

  private def linesBeforeMatch(linesBefore: Seq[String]): Boolean = {
    if (settings.hasLinesBeforePatterns) {
      linesMatch(linesBefore, settings.inLinesBeforePatterns,
        settings.outLinesBeforePatterns)
    } else {
      true
    }
  }

  private def linesAfterMatch(linesAfter: Seq[String]): Boolean = {
    if (settings.hasLinesAfterPatterns) {
      linesMatch(linesAfter, settings.inLinesAfterPatterns,
        settings.outLinesAfterPatterns)
    } else {
      true
    }
  }

  private def matchLinesAfterToOrUntil(linesAfter: mutable.ListBuffer[String],
                                       lines: Iterator[String]): Boolean = {
    if (settings.linesAfterToPatterns.isEmpty
      && settings.linesAfterUntilPatterns.isEmpty)
      return true

    linesAfter.zipWithIndex.foreach { li =>
      if (matchesAnyPattern(li._1, settings.linesAfterToPatterns)) {
        while (li._2 + 1 < linesAfter.size) linesAfter.remove(li._2 + 1)
        return true
      } else if (matchesAnyPattern(li._1, settings.linesAfterUntilPatterns)) {
        while (li._2 < linesAfter.size) linesAfter.remove(li._2)
        return true
      }
    }
    var foundMatch = false
    while (!foundMatch && lines.hasNext) {
      val nextLine = lines.next()
      if (matchesAnyPattern(nextLine, settings.linesAfterToPatterns)) {
        linesAfter += nextLine
        foundMatch = true
      } else if (matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns)) {
        foundMatch = true
      } else {
        linesAfter += nextLine
      }
    }
    foundMatch
  }

  private def searchTextFileSourceLines(sf: SearchFile, source: Source): Unit = {
    searchStringIterator(source.getLines()).foreach { r =>
      addSearchResult(r.copy(file=Some(sf)))
    }
  }

  def searchStringIterator(lines: Iterator[String]): Seq[SearchResult] = {
    val results = mutable.ArrayBuffer.empty[SearchResult]
    val linesBefore = new mutable.ListBuffer[String]
    val linesAfter = new mutable.ListBuffer[String]
    var lineNum: Int = 0
    val matchedPatterns = mutable.Set.empty[Regex]
    var stop = false
    while ((lines.hasNext || linesAfter.nonEmpty) && !stop) {
      lineNum += 1
      val line =
        if (linesAfter.nonEmpty) {
          linesAfter.remove(0)
        } else {
          lines.next()
        }
      if (settings.linesAfter > 0) {
        while (linesAfter.length < settings.linesAfter && lines.hasNext)
          linesAfter += lines.next
      }

      val searchPatterns =
        if (settings.firstMatch)
          settings.searchPatterns.diff(matchedPatterns)
        else settings.searchPatterns

      if (searchPatterns.isEmpty) {
        stop = true
      }

      searchPatterns.foreach { p =>
        val matchIterator: Iterator[Regex.Match] =
          if (settings.firstMatch) p.findFirstMatchIn(line).toIterator
          else p.findAllMatchIn(line)

        if (matchIterator.hasNext
          && linesBeforeMatch(linesBefore)
          && linesAfterMatch(linesAfter)
          && matchLinesAfterToOrUntil(linesAfter, lines.seq.seq)) {

          matchedPatterns.add(p)

          for (m <- matchIterator) {
            results += new SearchResult(
              p,
              None,
              lineNum,
              m.start + 1,
              m.end + 1,
              line,
              linesBefore.toList,
              linesAfter.toList)
          }
        }
      }

      if (settings.linesBefore > 0) {
        if (linesBefore.length == settings.linesBefore) {
          linesBefore.remove(0, 1)
        }
        if (linesBefore.length < settings.linesBefore) {
          linesBefore += line
        }
      }
    }
    Seq.empty[SearchResult] ++ results
  }

  private def searchBinaryFileSource(sf: SearchFile, source: Source): Unit = {
    if (settings.verbose) {
      Common.log("Searching binary file %s".format(sf.toString))
    }
    val contents = source.mkString
    source.close()
    for (p <- settings.searchPatterns) {
      val matchIterator =
        if (settings.firstMatch) p.findAllIn(contents).matchData.take(1)
        else p.findAllIn(contents).matchData
      for (m <- matchIterator) {
        addSearchResult(new SearchResult(
          p,
          Some(sf),
          0,
          m.start + 1,
          m.end + 1,
          null))
      }
    }
  }

  private def searchArchiveFileSource(sf: SearchFile, source: Source): Unit = {
    if (settings.verbose) {
      Common.log("Searching archive file %s".format(sf.toString))
    }
    if (FileTypes.isZipArchiveFile(sf)) {
      searchZipFileSource(sf, source)
    } else if (FileTypes.isGzArchiveFile(sf)) {
      searchGzFileSource(sf, source)
    } else if (FileTypes.isBz2ArchiveFile(sf)) {
      searchBz2FileSource(sf, source)
    } else if (FileTypes.isTarArchiveFile(sf)) {
      val tis = new BufferedInputStream(new FileInputStream(sf.getPath))
      searchTarFileInputStream(sf, tis)
    } else {
      Common.log("Currently unsupported archive file type: %s (%s)".
        format(FileUtil.getExtension(sf), sf.toString))
    }
  }

  private def searchZipFileSource(sf: SearchFile, source: Source): Unit = {
    if (settings.verbose) {
      Common.log("Searching zip file %s".format(sf.toString))
    }
    val zf = new ZipFile(sf.toFile)
    val entries = zf.entries().filterNot(_.isDirectory)
    val entryMap = mutable.LinkedHashMap.empty[String, List[ZipEntry]]
    entries foreach { ze =>
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
          val fileType = FileTypes.getFileType(file)
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

  private def searchTarFileInputStream(sf: SearchFile, is: InputStream): Unit = {
    if (settings.verbose) {
      Common.log("Searching tar file %s".format(sf.toString))
    }
    val tis = new TarArchiveInputStream(is)
    var entry: TarArchiveEntry = tis.getNextTarEntry
    while (entry != null) {
      if (!entry.isDirectory) {
        val dirName = new File(entry.getName).getParent
        if (isSearchDir(dirName)) {
          val file = new File(entry.getName)
          val fileType = FileTypes.getFileType(file)
          if (fileType != FileType.Unknown) {
            val bytes = new Array[Byte](entry.getSize.toInt)
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

  private def searchGzFileSource(sf: SearchFile, source: Source): Unit = {
    if (settings.verbose) {
      Common.log("Searching gzip file %s".format(sf.toString))
    }
    val containedFileName = sf.fileName.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(sf)
    val containedExtension = FileUtil.getExtension(sf)
    val gzsf = new SearchFile(sf.containers :+ sf.getPath, "", containedFileName,
      containedFileType)
    if (containedExtension == tarExtension || isSearchFile(gzsf.fileName)) {
      val gzis = new GZIPInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == tarExtension) {
        searchTarFileInputStream(gzsf, gzis)
      } else {
        val source = Source.fromInputStream(gzis)
        searchFileSource(gzsf, source)
      }
      gzis.close()
    }
  }

  private def searchBz2FileSource(sf: SearchFile, source: Source): Unit = {
    if (settings.verbose) {
      Common.log("Searching bzip2 file %s".format(sf.toString))
    }
    val containedFileName = sf.fileName.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(sf)
    val containedExtension = FileUtil.getExtension(sf)
    val bzsf = new SearchFile(sf.containers :+ sf.getPath, "", containedFileName,
      containedFileType)
    if (containedExtension == tarExtension || isSearchFile(bzsf.fileName)) {
      val bzis = new BZip2CompressorInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == tarExtension) {
        searchTarFileInputStream(bzsf, bzis)
      } else {
        val source = Source.fromInputStream(bzis)
        searchFileSource(bzsf, source)
      }
      bzis.close()
    }
  }

  private def addSearchResult(r: SearchResult): Unit = {
    _searchResults.append(r)
  }

  private def cmpSearchResults(r1: SearchResult, r2: SearchResult): Boolean = {
    var (path1, fileName1, path2, fileName2) = ("", "", "", "")
    if (r1.file.isDefined) {
      val file1 = r1.file.get.toFile
      path1 = file1.getParent.toLowerCase
      fileName1 = file1.getName.toLowerCase
    }
    if (r2.file.isDefined) {
      val file2 = r2.file.get.toFile
      path2 = file2.getParent.toLowerCase
      fileName2 = file2.getName.toLowerCase
    }
    if (path1 == path2) {
      if (fileName1 == fileName2) {
        if (r1.lineNum == r2.lineNum) {
          r1.matchStartIndex < r2.matchStartIndex
        } else {
          r1.lineNum < r2.lineNum
        }
      } else {
        fileName1 < fileName2
      }
    } else {
      path1 < path2
    }
  }

  def getSearchResults: Seq[SearchResult] = {
    _searchResults.sortWith(cmpSearchResults).toList
  }

  def printSearchResults(): Unit = {
    getSearchResults.foreach(printSearchResult)
 }

  private def printSearchResult(r: SearchResult): Unit = {
    val patternString = if (settings.searchPatterns.size > 1) {
      "\"" + r.searchPattern + "\": "
    } else {
      ""
    }
    Common.log(patternString + r.toString)
  }

  // retrieve only AFTER search
  lazy val searchResults: Seq[SearchResult] = _searchResults.sortWith(cmpSearchResults).toVector

  def getMatchingDirs: Seq[File] = {
    searchResults.filter(_.file.isDefined).map(_.file.get.toFile.getParentFile).
      distinct.toVector
  }

  def getMatchingFiles: Seq[File] = {
    searchResults.filter(_.file.isDefined).map(_.file.get.toFile).distinct.
      toVector
  }

  def getMatchingLines: Seq[String] = {
    val allLines = searchResults.flatMap(r => Option(r.line)).map(_.trim)
    if (settings.uniqueLines) {
      allLines.distinct.sortWith(_.toUpperCase < _.toUpperCase)
    } else {
      allLines.sortWith(_.toUpperCase < _.toUpperCase)
    }
  }
}

// doing this so I can test methods in Searcher class that can be static
object Searcher {
  def getLineIndices(contents:String): Seq[(Int,Int)] = {
    val newLineIndices = contents.zipWithIndex.collect { case ('\n',i) => i }
    if (newLineIndices.nonEmpty) {
      val lineIndices = mutable.ArrayBuffer[(Int,Int)]((0,newLineIndices.head))
      lineIndices ++= newLineIndices.map(_ + 1).zip(newLineIndices.tail :+ contents.length)
      lineIndices
    } else {
      Seq[(Int,Int)]((0,contents.length-1))
    }
  }
}
