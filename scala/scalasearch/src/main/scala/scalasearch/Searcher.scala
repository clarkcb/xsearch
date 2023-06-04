package scalasearch

import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import scalafind.Common.log
import scalafind.{FileResult, FileType, FileTypes, Finder}
import scalafind.FileType.FileType

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.nio.file.Paths
import java.util.zip.{GZIPInputStream, ZipFile}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex
import scala.util.control.NonLocalReturns.*

class Searcher (_settings: SearchSettings) {
  import scalafind.FileUtil._
  import Searcher._

  val settings: SearchSettings = _settings
  private val finder: Finder = try {
    Finder(_settings.findSettings)
  } catch {
    case e: Exception => throw new SearchException(e.getMessage)
    case _ => throw new SearchException("An unknown error occurred trying to create Finder")
  }

  private def validateSettings(): Unit = {
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

  def search(): Seq[SearchResult] = {

    val fileResults = finder.find()

    if (settings.verbose) {
      val dirs = fileResults.map(f => pathOrCurrent(f.path.getParent))
        .map(_.toString).distinct.sorted
      log("\nDirectories to be searched (%d):\n%s".format(dirs.size,
        dirs.mkString("\n")))
      log("\nFiles to be searched (%d):\n%s".format(fileResults.size,
        fileResults.map(_.toString).mkString("\n")))
      log("\nStarting file search...\n")
    }
    val results: Seq[SearchResult] = searchFiles(fileResults)
    if (settings.verbose) {
      log("\nFile search complete.\n")
    }

    results
  }

  def searchFiles(files: Seq[FileResult]): Seq[SearchResult] = {
    var offset = 0
    val batchSize = 1000
    var until = batchSize

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

  def batchSearchFiles(files: Seq[FileResult]): Seq[SearchResult] = {
    import java.util.concurrent.ConcurrentHashMap

    val searchFileResultsMap: ConcurrentHashMap[String, Seq[SearchResult]] = new ConcurrentHashMap

    files.map { f =>
      Future {
        searchFile(f)
      }.map { results =>
        searchFileResultsMap put (f.path.toString, results)
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

  def searchFile(fr: FileResult): Seq[SearchResult] = {
    FileTypes.getFileType(fr.path.getFileName.toString) match {
      case ft if Set(FileType.Text, FileType.Code, FileType.Xml).contains(ft) =>
        searchTextFileSource(fr, Source.fromFile(fr.path.toFile, settings.textFileEncoding))
      case FileType.Binary =>
        searchBinaryFileSource(fr, Source.fromFile(fr.path.toFile, "ISO-8859-1"))
      case FileType.Archive =>
        searchArchiveFileSource(fr, Source.fromFile(fr.path.toFile))
      case _ =>
        log("Skipping unknown file type: %s".format(fr))
        Seq.empty[SearchResult]
    }
  }

  private def searchFileSource(fr: FileResult, source: Source): Seq[SearchResult] = {
    FileTypes.getFileType(fr.path.getFileName.toString) match {
      case ft if Set(FileType.Code, FileType.Text, FileType.Xml).contains(ft) =>
        searchTextFileSource(fr, source)
      case FileType.Binary =>
        searchBinaryFileSource(fr, source)
      case FileType.Archive =>
        searchArchiveFileSource(fr, source)
      case _ =>
        log("Skipping unknown file type: %s".format(fr))
        Seq.empty[SearchResult]
    }
  }

  private def searchTextFileSource(fr: FileResult, source: Source): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching text file %s".format(fr.toString))
    }
    if (settings.multiLineSearch) {
      searchTextFileSourceContents(fr, source)
    } else {
      searchTextFileSourceLines(fr, source)
    }
  }

  private def searchTextFileSourceContents(fr: FileResult, source: Source): Seq[SearchResult] = {
    try {
      val contents = source.mkString
      searchMultiLineString(contents).map { r =>
        r.copy(file = Some(fr))
      }
    } catch {
      case _: java.nio.charset.MalformedInputException =>
        if (settings.verbose) {
          log(s"Skipping file with unsupported encoding: $fr")
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
                                       lines: Iterator[String]): Boolean = returning {
    if (settings.hasLinesAfterToOrUntilPatterns) {
      linesAfter.zipWithIndex.foreach { li =>
        if (matchesAnyPattern(li._1, settings.linesAfterToPatterns)) {
          while (li._2 + 1 < linesAfter.size) linesAfter.remove(li._2 + 1)
          throwReturn(true)
        } else if (matchesAnyPattern(li._1, settings.linesAfterUntilPatterns)) {
          while (li._2 < linesAfter.size) linesAfter.remove(li._2)
          throwReturn(true)
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

  private def searchTextFileSourceLines(fr: FileResult, source: Source): Seq[SearchResult] = {
    try {
      searchStringIterator(source.getLines()).map { r =>
        r.copy(file=Some(fr))
      }
    } catch {
      case _: java.nio.charset.MalformedInputException =>
        if (settings.verbose) {
          log(s"Skipping file with unsupported encoding: $fr")
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

  private def searchBinaryFileSource(fr: FileResult, source: Source): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching binary file %s".format(fr.toString))
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
      matchIterator.iterator.foreach(m => {
        results += new SearchResult(p, Some(fr), 0, m.start + 1, m.end + 1, None)
      })
    }
    Seq.empty[SearchResult] ++ results
  }

  private def searchArchiveFileSource(fr: FileResult, source: Source): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching archive file %s".format(fr.toString))
    }
    if (FileTypes.isZipArchiveFile(fr)) {
      searchZipFile(fr)
    } else if (FileTypes.isGzArchiveFile(fr)) {
      searchGzFile(fr)
    } else if (FileTypes.isBz2ArchiveFile(fr)) {
      searchBz2File(fr)
    } else if (FileTypes.isTarArchiveFile(fr)) {
      val tis = new BufferedInputStream(new FileInputStream(fr.path.toFile))
      searchTarFileInputStream(fr, tis)
    } else {
      log("Currently unsupported archive file type: %s (%s)".
        format(getExtension(fr), fr.toString))
      Seq.empty[SearchResult]
    }
  }

  private def searchZipFile(fr: FileResult): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching zip file %s".format(fr.toString))
    }
    val zf = new ZipFile(fr.path.toFile)
    zf.entries().asScala.filterNot(_.isDirectory)
      .map(e => new File(e.getName))
      .filter { f =>
        Option(f.getParent) match {
          case Some(d) => isSearchDir(d)
          case None => true
        }
      }
      //.filter(filterFile)
      .flatMap { f =>
        val fileType = FileTypes.getFileType(f.getName)
        if (fileType != FileType.Unknown) {
          val zsf = new FileResult(fr.containers :+ fr.path.toString, f.toPath, fileType)
          val zis = zf.getInputStream(zf.getEntry(zsf.path.toString))
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

  private def searchTarFileInputStream(fr: FileResult, is: InputStream): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching tar file %s".format(fr.toString))
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
              val tzsf = new FileResult(fr.containers, file.toPath, fileType)
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

  private def searchGzFile(fr: FileResult): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching gzip file %s".format(fr.toString))
    }
    val containedFileName = fr.path.getFileName.toString.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(fr.path.getFileName.toString)
    val containedExtension = getExtension(fr)
    val gzsf = new FileResult(fr.containers :+ fr.path.toString, Paths.get(containedFileName),
      containedFileType)
    val gzResults = mutable.ListBuffer[SearchResult]()
    if (containedExtension == tarExtension || finder.isMatchingFileResult(gzsf)) {
      val gzis = new GZIPInputStream(new BufferedInputStream(
        new FileInputStream(fr.path.toFile)))
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

  private def searchBz2File(fr: FileResult): Seq[SearchResult] = {
    if (settings.verbose) {
      log("Searching bzip2 file %s".format(fr.toString))
    }
    val containedFileName = fr.path.getFileName.toString.split("\\.").init.mkString(currentPath)
    val containedFileType = FileTypes.getFileType(fr.path.getFileName.toString)
    val containedExtension = getExtension(fr)
    val bzsf = new FileResult(fr.containers :+ fr.path.toString, Paths.get(containedFileName),
      containedFileType)
    val bzResults = mutable.ListBuffer[SearchResult]()
    if (containedExtension == tarExtension || finder.isMatchingFileResult(bzsf)) {
      val bzis = new BZip2CompressorInputStream(new BufferedInputStream(
        new FileInputStream(fr.path.toFile)))
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

  def compareResults(sr1: SearchResult, sr2: SearchResult): Boolean = {
    val fileResultCmp = (sr1.file, sr2.file) match {
      case (Some(fr1), Some(fr2)) => finder.cmpFileResults(fr1, fr2)
      case _ => false
    }
    if (fileResultCmp) {
      val lineNumCmp = sr1.lineNum.compareTo(sr2.lineNum)
      if (lineNumCmp == 0) {
        sr1.matchStartIndex.compareTo(sr2.matchStartIndex) < 0
      } else {
        lineNumCmp < 0
      }
    } else {
      fileResultCmp
    }
  }
}

object Searcher {
  val currentPath = "."
  val tarExtension = "tar"

  val settingsTests: Seq[SearchSettings => Option[String]] = Seq[SearchSettings => Option[String]](
    ss => if (ss.paths.nonEmpty) None else Some("Startpath not defined"),
    ss => if (ss.paths.forall { p => new File(p).exists() }) None else Some("Startpath not found"),
    ss => if (ss.paths.forall { p => new File(p).canRead }) None else Some("Startpath not readable"),
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

  def getLineIndices(contents: String): Seq[(Int,Int)] = {
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
