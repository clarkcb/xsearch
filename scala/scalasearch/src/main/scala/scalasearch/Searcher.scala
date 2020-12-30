package scalasearch

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scalasearch.Common.log
import scalasearch.FileType.FileType

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.util.zip.{GZIPInputStream, ZipFile}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

class Searcher (settings: SearchSettings) {
  import FileUtil._
  import Searcher._

  def validateSettings(): Unit = {
    settingsTests.foreach { t =>
      t(settings) match {
        case Some(err) => throw new SearchException(err)
        case _ =>
      }
    }
    try {
      val _ = Charset.forName(settings.textFileEncoding)
    } catch {
      case _: IllegalArgumentException =>
        throw new SearchException(s"Invalid encoding: ${settings.textFileEncoding}")
    }
  }
  validateSettings()

  def isSearchDir(d: File): Boolean = {
    isSearchDir(d.getName)
  }

  def isSearchDir(dirName: String): Boolean = {
    val pathElems = splitPath(dirName)
    if (pathElems.exists(p => isHidden(p)) && settings.excludeHidden) {
      false
    } else {
      filterByPatterns(dirName, settings.inDirPatterns, settings.outDirPatterns)
    }
  }

  def isSearchFile(sf: SearchFile): Boolean = {
    isSearchFile(sf.file.getPath, sf.fileType)
  }

  def isSearchFile(fileName: String, fileType: FileType): Boolean = {
    val fileNameTests = Seq[String => Boolean](
      fileName => !isHidden(fileName) || !settings.excludeHidden,
      fileName => settings.inExtensions.isEmpty ||
        settings.inExtensions.contains(getExtension(fileName)),
      fileName => settings.outExtensions.isEmpty ||
        !settings.outExtensions.contains(getExtension(fileName)),
    )
    val fileTypeTests = Seq[FileType => Boolean](
      fileType => settings.inFileTypes.isEmpty ||
        settings.inFileTypes.contains(fileType),
      fileType => settings.outFileTypes.isEmpty ||
        !settings.outFileTypes.contains(fileType),
    )

    fileNameTests.forall(t => t(fileName)) &&
      fileTypeTests.forall(t => t(fileType)) &&
      filterByPatterns(fileName, settings.inFilePatterns, settings.outFilePatterns)
  }

  def isArchiveSearchFile(fileName: String): Boolean = {
    val fileNameTests = Seq[String => Boolean](
      fileName => !isHidden(fileName) || !settings.excludeHidden,
      fileName => settings.inArchiveExtensions.isEmpty ||
        settings.inArchiveExtensions.contains(getExtension(fileName)),
      fileName => settings.outArchiveExtensions.isEmpty ||
        !settings.outArchiveExtensions.contains(getExtension(fileName)),
    )
    fileNameTests.forall(t => t(fileName)) &&
      filterByPatterns(fileName, settings.inArchiveFilePatterns, settings.outArchiveFilePatterns)
  }

  def filterFile(f: File): Boolean = {
    val fileType = FileTypes.getFileType(f.getName)
    fileType match {
      case FileType.Unknown => false
      case FileType.Archive =>
        settings.searchArchives && isArchiveSearchFile(f.getName)
      case _ =>
        !settings.archivesOnly && isSearchFile(f.getName, fileType)
    }
  }

  def filterToSearchFile(f: File): Option[SearchFile] = {
    val fileType = FileTypes.getFileType(f.getName)
    fileType match {
      case FileType.Unknown => None
      case FileType.Archive =>
        if (settings.searchArchives && isArchiveSearchFile(f.getName)) {
          Some(new SearchFile(f, fileType))
        } else {
          None
        }
      case _ =>
        if (!settings.archivesOnly && isSearchFile(f.getPath, fileType)) {
          Some(new SearchFile(f, fileType))
        } else {
          None
        }
    }
  }

  final def getSearchFiles(startPathFile: File): Seq[SearchFile] = {
    val files = startPathFile.listFiles
    files.filter(_.isFile).flatMap(filterToSearchFile) ++
      files
        .filter(_.isDirectory)
        .filter(_ => settings.recursive)
        .filter(isSearchDir)
        .flatMap(getSearchFiles)
  }

  def search(): Seq[SearchResult] = {
    val startPathFile = new File(settings.startPath.get)
    if (startPathFile.isDirectory) {
      if (isSearchDir(startPathFile)) {
        searchPath(startPathFile)
      } else {
        throw new SearchException("Startpath does not match search settings")
      }
    } else if (startPathFile.isFile) {
      if (filterFile(startPathFile)) {
        val fileType = FileTypes.getFileType(startPathFile.getName)
        searchFile(new SearchFile(startPathFile, fileType))
      } else {
        throw new SearchException("Startpath does not match search settings")
      }
    } else {
      throw new SearchException("Startpath not searchable")
    }
  }

  def searchPath(startPath: File): Seq[SearchResult] = {
    val files: Seq[SearchFile] = getSearchFiles(startPath)
    if (settings.verbose) {
      val dirs = files.map(f => pathOrCurrent(f.file.getParentFile))
        .map(_.getPath).distinct.sorted
      log("\nDirectories to be searched (%d):\n%s".format(dirs.size,
        dirs.mkString("\n")))
      log("\nFiles to be searched (%d):\n%s".format(files.size,
        files.mkString("\n")))
      log("\nStarting file search...\n")
    }
    val results: Seq[SearchResult] = searchFiles(files)
    if (settings.verbose) {
      log("\nFile search complete.\n")
    }
    results
  }

  def searchFiles(files: Seq[SearchFile]): Seq[SearchResult] = {
    var offset = 0
    val batchSize = 1000
    var until = offset + batchSize

    if (files.length > batchSize) {
      val results = mutable.ListBuffer.empty[SearchResult]
      while (offset < files.length) {
        results ++= batchSearchFiles(files.slice(offset, until))
        offset = math.min(offset + batchSize, files.length)
        until = offset + batchSize
      }
      Seq.empty[SearchResult] ++ results

    } else {
      files.flatMap { f =>
        searchFile(f)
      }
    }
  }

  def batchSearchFiles(files: Seq[SearchFile]): Seq[SearchResult] = {
    import java.util.concurrent.ConcurrentHashMap

    val searchFileResultsMap: ConcurrentHashMap[String, Seq[SearchResult]] = new ConcurrentHashMap

    files.map { f =>
      Future {
        searchFile(f)
      }.map { results =>
        searchFileResultsMap put (f.getPath, results)
      }
    }

    while (searchFileResultsMap.size() < files.length) {
      Thread.sleep(500)
    }

    val results = mutable.ListBuffer.empty[SearchResult]
    for (k <- searchFileResultsMap.asScala.keySet) {
      results ++= searchFileResultsMap.get(k)
    }
    Seq.empty[SearchResult] ++ results
  }

  def searchFile(sf: SearchFile): Seq[SearchResult] = {
    FileTypes.getFileType(sf.file.getName) match {
      case ft if Set(FileType.Text, FileType.Code, FileType.Xml).contains(ft) =>
        searchTextFileSource(sf, Source.fromFile(sf.file, settings.textFileEncoding))
      case FileType.Binary =>
        searchBinaryFileSource(sf, Source.fromFile(sf.file, "ISO-8859-1"))
      case FileType.Archive =>
        searchArchiveFileSource(sf, Source.fromFile(sf.file))
      case _ =>
        log("Skipping unknown file type: %s".format(sf))
        Seq.empty[SearchResult]
    }
  }

  private def searchFileSource(sf: SearchFile, source: Source): Seq[SearchResult] = {
    FileTypes.getFileType(sf.file.getName) match {
      case ft if Set(FileType.Code, FileType.Text, FileType.Xml).contains(ft) =>
        searchTextFileSource(sf, source)
      case FileType.Binary =>
        searchBinaryFileSource(sf, source)
      case FileType.Archive =>
        searchArchiveFileSource(sf, source)
      case _ =>
        log("Skipping unknown file type: %s".format(sf))
        Seq.empty[SearchResult]
    }
  }

  private def searchTextFileSource(sf: SearchFile, source: Source): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching text file %s".format(sf.getPathWithContainers))
    }
    if (settings.multiLineSearch) {
      searchTextFileSourceContents(sf, source)
    } else {
      searchTextFileSourceLines(sf, source)
    }
  }

  private def searchTextFileSourceContents(sf: SearchFile, source: Source): Seq[SearchResult] = {
    try {
      val contents = source.mkString
      searchMultiLineString(contents).map { r =>
        r.copy(file = Some(sf))
      }
    } catch {
      case _: java.nio.charset.MalformedInputException =>
        if (settings.verbose) {
          log(s"Skipping file with unsupported encoding: $sf")
        }
        Seq.empty[SearchResult]
    }
  }

  def searchMultiLineString(s: String): Seq[SearchResult] = {
    settings.searchPatterns.flatMap { p =>
      searchMultiLineStringForPattern(s, p)
    }.toSeq
  }

  private def searchMultiLineStringForPattern(s: String, p: Regex): Seq[SearchResult] = {
    val lineIndices: Seq[(Int, Int)] = Searcher.getLineIndices(s)
    val results = mutable.ArrayBuffer.empty[SearchResult]
    val matches = p.findAllIn(s).matchData
    var stop = false

    def getLinesAfterFromMultiLineString(s: String, startIndex: Int,
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
        results += SearchResult(
          p,
          None,
          beforeLineCount + 1,
          m.start - thisLineIndices._1 + 1,
          m.end - thisLineIndices._1 + 1,
          Some(line),
          linesBefore,
          linesAfter)
        if (settings.firstMatch) {
          stop = true
        }
      }
    }
    results.toSeq
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
    if (settings.hasLinesAfterToOrUntilPatterns) {
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
    } else {
      true
    }
  }

  private def searchTextFileSourceLines(sf: SearchFile, source: Source): Seq[SearchResult] = {
    try {
      searchStringIterator(source.getLines()).map { r =>
        r.copy(file=Some(sf))
      }
    } catch {
      case _: java.nio.charset.MalformedInputException =>
        if (settings.verbose) {
          log(s"Skipping file with unsupported encoding: $sf")
        }
        Seq.empty[SearchResult]
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
          if (settings.firstMatch) p.findFirstMatchIn(line).iterator
          else p.findAllMatchIn(line)

        if (matchIterator.hasNext
          && linesBeforeMatch(linesBefore.toSeq)
          && linesAfterMatch(linesAfter.toSeq)
          && matchLinesAfterToOrUntil(linesAfter, lines)) {

          matchedPatterns.add(p)

          for (m <- matchIterator) {
            results += SearchResult(
              p,
              None,
              lineNum,
              m.start + 1,
              m.end + 1,
              Some(line),
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

  private def searchBinaryFileSource(sf: SearchFile, source: Source): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching binary file %s".format(sf.toString))
    }
    val contents = source.mkString
    source.close()
    var results = mutable.ListBuffer[SearchResult]()
    for (p <- settings.searchPatterns) {
      val matchIterator =
        if (settings.firstMatch) {
          p.findFirstMatchIn(contents)
        } else {
          p.findAllMatchIn(contents)
        }
      for (m <- matchIterator) {
        results += new SearchResult(p, Some(sf), 0, m.start + 1, m.end + 1, None)
      }
    }
    Seq.empty[SearchResult] ++ results
  }

  private def searchArchiveFileSource(sf: SearchFile, source: Source): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching archive file %s".format(sf.toString))
    }
    if (FileTypes.isZipArchiveFile(sf)) {
      searchZipFile(sf)
    } else if (FileTypes.isGzArchiveFile(sf)) {
      searchGzFile(sf)
    } else if (FileTypes.isBz2ArchiveFile(sf)) {
      searchBz2File(sf)
    } else if (FileTypes.isTarArchiveFile(sf)) {
      val tis = new BufferedInputStream(new FileInputStream(sf.getPath))
      searchTarFileInputStream(sf, tis)
    } else {
      log("Currently unsupported archive file type: %s (%s)".
        format(getExtension(sf), sf.toString))
      Seq.empty[SearchResult]
    }
  }

  private def searchZipFile(sf: SearchFile): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching zip file %s".format(sf.toString))
    }
    val zf = new ZipFile(sf.file)
    zf.entries().asScala.filterNot(_.isDirectory)
      .map(e => new File(e.getName))
      .filter { f =>
        Option(f.getParent) match {
          case Some(d) => isSearchDir(d)
          case None => true
        }
      }
      .filter(filterFile)
      .flatMap { f =>
        val fileType = FileTypes.getFileType(f.getName)
        if (fileType != FileType.Unknown) {
          val zsf = new SearchFile(sf.containers :+ sf.getPath, f, fileType)
          val zis = zf.getInputStream(zf.getEntry(zsf.getPath))
          val entryResults = fileType match {
            case FileType.Archive =>
              searchArchiveFileSource(zsf, Source.fromInputStream(zis))
            case _ =>
              searchFileSource(zsf, Source.fromInputStream(zis))
          }
          zis.close()
          entryResults
        } else {
          Seq.empty[SearchResult]
        }
      }.toSeq
  }

  private def searchTarFileInputStream(sf: SearchFile, is: InputStream): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching tar file %s".format(sf.toString))
    }
    val tarResults = mutable.ListBuffer[SearchResult]()
    val tis = new TarArchiveInputStream(is)
    var entry: TarArchiveEntry = tis.getNextTarEntry
    while (entry != null) {
      if (!entry.isDirectory) {
        val dirName = new File(entry.getName).getParent
        if (isSearchDir(dirName)) {
          val file = new File(entry.getName)
          val fileType = FileTypes.getFileType(file.getName)
          if (fileType != FileType.Unknown) {
            val bytes = new Array[Byte](entry.getSize.toInt)
            val count = tis.read(bytes, 0, entry.getSize.toInt)
            if (count > 0) {
              val tzsf = new SearchFile(sf.containers, file, fileType)
              val source = Source.fromBytes(bytes)
              fileType match {
                case FileType.Archive =>
                  tarResults ++= searchArchiveFileSource(tzsf, source)
                case _ =>
                  tarResults ++= searchFileSource(tzsf, source)
              }
            }
          }
        }
      }
      entry = tis.getNextTarEntry
    }
    Seq.empty[SearchResult] ++ tarResults
  }

  private def searchGzFile(sf: SearchFile): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching gzip file %s".format(sf.toString))
    }
    val containedFileName = sf.file.getName.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(sf.file.getName)
    val containedExtension = getExtension(sf)
    val gzsf = new SearchFile(sf.containers :+ sf.getPath, new File(containedFileName),
      containedFileType)
    val gzResults = mutable.ListBuffer[SearchResult]()
    val fileType = FileTypes.getFileType(gzsf.file.getName)
    if (containedExtension == tarExtension || isSearchFile(gzsf.file.getName, fileType)) {
      val gzis = new GZIPInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == tarExtension) {
        gzResults ++= searchTarFileInputStream(gzsf, gzis)
      } else {
        val source = Source.fromInputStream(gzis)
        gzResults ++= searchFileSource(gzsf, source)
      }
      gzis.close()
    }
    Seq.empty[SearchResult] ++ gzResults
  }

  private def searchBz2File(sf: SearchFile): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching bzip2 file %s".format(sf.toString))
    }
    val containedFileName = sf.file.getName.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(sf.file.getName)
    val containedExtension = getExtension(sf)
    val bzsf = new SearchFile(sf.containers :+ sf.getPath, new File(containedFileName),
      containedFileType)
    val bzResults = mutable.ListBuffer[SearchResult]()
    val fileType = FileTypes.getFileType(bzsf.file.getName)
    if (containedExtension == tarExtension || isSearchFile(bzsf.file.getName, fileType)) {
      val bzis = new BZip2CompressorInputStream(new BufferedInputStream(
        new FileInputStream(sf.getPath)))
      if (containedExtension == tarExtension) {
        bzResults ++= searchTarFileInputStream(bzsf, bzis)
      } else {
        val source = Source.fromInputStream(bzis)
        bzResults ++= searchFileSource(bzsf, source)
      }
      bzis.close()
    }
    Seq.empty[SearchResult] ++ bzResults
  }
}

object Searcher {
  val currentPath = "."
  val tarExtension = "tar"

  val settingsTests: Seq[SearchSettings => Option[String]] = Seq[SearchSettings => Option[String]](
    ss => if (ss.startPath.isDefined && ss.startPath.get.nonEmpty) None else Some("Startpath not defined"),
    ss => if (new File(ss.startPath.get).exists()) None else Some("Startpath not found"),
    ss => if (new File(ss.startPath.get).canRead) None else Some("Startpath not readable"),
    ss => if (ss.searchPatterns.nonEmpty) None else Some("No search patterns defined"),
    ss => if (ss.linesAfter >= 0) None else Some("Invalid linesafter"),
    ss => if (ss.linesBefore >= 0) None else Some("Invalid linesbefore"),
    ss => if (ss.maxLineLength >= 0) None else Some("Invalid maxlinelength"),
  )

  def listToString(stringList: Iterable[Any]): String = {
    stringList.mkString("[\"", "\", \"", "\"]")
  }

  def matchesAnyPattern(s: String, patterns: Set[Regex]): Boolean = {
    patterns exists (_.findFirstMatchIn(s).isDefined)
  }

  def anyMatchesAnyPattern(strings: Seq[String], patterns: Set[Regex]): Boolean = {
    strings exists (matchesAnyPattern(_, patterns))
  }

  def filterByPatterns(s: String, inPatterns: Set[Regex], outPatterns: Set[Regex]): Boolean = {
    ((inPatterns.isEmpty || matchesAnyPattern(s, inPatterns))
      &&
      (outPatterns.isEmpty || !matchesAnyPattern(s, outPatterns)))
  }

  def linesMatch(lines: Seq[String], inPatterns: Set[Regex],
                         outPatterns: Set[Regex]): Boolean = {
    (inPatterns.isEmpty || anyMatchesAnyPattern(lines, inPatterns)) &&
      (outPatterns.isEmpty || !anyMatchesAnyPattern(lines, outPatterns))
  }

  def getLineIndices(contents:String): Seq[(Int,Int)] = {
    val newLineIndices = contents.zipWithIndex.collect { case ('\n',i) => i }
    if (newLineIndices.nonEmpty) {
      val lineIndices = mutable.ArrayBuffer[(Int,Int)]((0,newLineIndices.head))
      lineIndices ++= newLineIndices.map(_ + 1).zip(newLineIndices.tail :+ contents.length)
      lineIndices.toSeq
    } else {
      Seq[(Int,Int)]((0,contents.length-1))
    }
  }
}
