package scalasearch

import java.io.File

object SearchMain {

  private def cmpSearchResults(r1: SearchResult, r2: SearchResult): Boolean = {
    val (path1, fileName1) = r1.file match {
      case Some(file1) =>
        (FileUtil.pathOrCurrent(file1.file.getParentFile).getPath, file1.file.getName.toLowerCase)
      case None => ("", "")
    }
    val (path2, fileName2) = r2.file match {
      case Some(file2) =>
        (FileUtil.pathOrCurrent(file2.file.getParentFile).getPath, file2.file.getName.toLowerCase)
      case None => ("", "")
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

  def printSearchResults(results: Seq[SearchResult], settings: SearchSettings): Unit = {
    // TODO: add includePattern setting in formatted output
    val formatter = new SearchResultFormatter(settings)
    results.sortWith(cmpSearchResults).foreach(r => Common.log(formatter.format(r)))
  }

  def getMatchingDirs(results: Seq[SearchResult]): Seq[File] = {
    results
      .filter(_.file.isDefined)
      .map(r => FileUtil.pathOrCurrent(r.file.get.file.getParentFile))
      .distinct
      .toVector
  }

  def getMatchingFiles(results: Seq[SearchResult]): Seq[File] = {
    results
      .filter(_.file.isDefined)
      .map(_.file.get.file)
      .distinct
      .toVector
  }

  def getMatchingLines(results: Seq[SearchResult], settings: SearchSettings): Seq[String] = {
    val allLines = results.flatMap(r => r.line).map(_.trim)
    if (settings.uniqueLines) {
      allLines.distinct.sortWith(_.toUpperCase < _.toUpperCase)
    } else {
      allLines.sortWith(_.toUpperCase < _.toUpperCase)
    }
  }

  def printMatchingDirs(results: Seq[SearchResult]): Unit = {
    val dirs = getMatchingDirs(results)
    Common.log("\nDirectories with matches (%d):".format(dirs.length))
    dirs.foreach(f => Common.log(f.toString))
  }

  def printMatchingFiles(results: Seq[SearchResult]): Unit = {
    val files = getMatchingFiles(results)
    Common.log("\nFiles with matches (%d):".format(files.length))
    files.foreach(f => Common.log(f.toString))
  }

  def printMatchingLines(results: Seq[SearchResult], settings: SearchSettings): Unit = {
    val lines = getMatchingLines(results, settings)
    val hdr =
      if (settings.uniqueLines) {
        "\nUnique lines with matches (%d):"
      } else {
        "\nLines with matches (%d):"
      }
    Common.log(hdr.format(lines.length))
    lines.foreach(Common.log)
  }

  def main(args: Array[String]): Unit = {
    try {
      val settings = SearchOptions.settingsFromArgs(args)

      if (settings.debug) {
        Common.log("settings: " + settings)
      }

      if (settings.printUsage) {
        Common.log("")
        SearchOptions.usage(0)
      }

      val searcher = new Searcher(settings)
      val results = searcher.search()

      if (settings.printResults) {
        Common.log("\nSearch results (%d):".format(results.length))
        printSearchResults(results, settings)
      }
      if (settings.listDirs) { printMatchingDirs(results) }
      if (settings.listFiles) { printMatchingFiles(results) }
      if (settings.listLines) { printMatchingLines(results, settings) }

    } catch {
      case e: SearchException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        SearchOptions.usage(1)
    }
  }
}
