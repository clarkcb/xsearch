package scalasearch

import scalafind.{Common, FileUtil}

import java.io.File

object SearchMain {

  private def printSearchResults(results: Seq[SearchResult], searcher: Searcher): Unit = {
    // TODO: add includePattern setting in formatted output
    val formatter = new SearchResultFormatter(searcher.settings)
    results.sortWith((sr1, sr2) => searcher.compareResults(sr1, sr2))
      .foreach(r => Common.log(formatter.format(r)))
  }

  private def getMatchingDirs(results: Seq[SearchResult]): Seq[String] = {
    results
      .filter(_.file.isDefined)
      .map(r => FileUtil.pathOrCurrent(r.file.get.path.getParent).toString)
      .distinct
      .toVector
  }

  private def getMatchingFiles(results: Seq[SearchResult]): Seq[String] = {
    results
      .filter(_.file.isDefined)
      .map(_.file.get.path.getFileName.toString)
      .distinct
      .toVector
  }

  private def getMatchingLines(results: Seq[SearchResult], settings: SearchSettings): Seq[String] = {
    val allLines = results.flatMap(r => r.line).map(_.trim)
    if (settings.uniqueLines) {
      allLines.distinct.sortWith(_.toUpperCase < _.toUpperCase)
    } else {
      allLines.sortWith(_.toUpperCase < _.toUpperCase)
    }
  }

  private def printMatchingDirs(results: Seq[SearchResult]): Unit = {
    val dirs = getMatchingDirs(results)
    Common.log("\nDirectories with matches (%d):".format(dirs.length))
    dirs.foreach(f => Common.log(f))
  }

  private def printMatchingFiles(results: Seq[SearchResult]): Unit = {
    val files = getMatchingFiles(results)
    Common.log("\nFiles with matches (%d):".format(files.length))
    files.foreach(f => Common.log(f))
  }

  private def printMatchingLines(results: Seq[SearchResult], settings: SearchSettings): Unit = {
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
        printSearchResults(results, searcher)
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
