package scalasearch

import scalafind.{Common, FileUtil}

import java.nio.file.Path

object SearchMain {

  private def printSearchResults(results: Seq[SearchResult], searcher: Searcher): Unit = {
    // TODO: add includePattern setting in formatted output
    if (results.isEmpty) {
      Common.log("\nSearch results: 0")
    } else {
      Common.log("\nSearch results (%d):".format(results.length))
      val formatter = new SearchResultFormatter(searcher.settings)
      results.sortWith((sr1, sr2) => searcher.compareResults(sr1, sr2))
        .foreach(r => Common.log(formatter.format(r)))
    }
  }

  private def getMatchingDirs(results: Seq[SearchResult]): Seq[Path] = {
    results
      .filter(_.file.isDefined)
      .map(r => FileUtil.pathOrCurrent(r.file.get.path))
      .distinct
      .sortWith(_.toString < _.toString)
  }

  private def getMatchingFiles(results: Seq[SearchResult]): Seq[String] = {
    results
      .filter(_.file.isDefined)
      .map(_.file.get.path.toString)
      .distinct
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
    if (dirs.nonEmpty) {
      Common.log("\nMatching directories (%d):".format(dirs.length))
      dirs.foreach(d => Common.log(d.toString))
    } else {
      Common.log("\nMatching directories: 0")
    }
  }

  private def printMatchingFiles(results: Seq[SearchResult]): Unit = {
    val files = getMatchingFiles(results)
    if (files.nonEmpty) {
      Common.log("\nMatching files (%d):".format(files.length))
      files.foreach(f => Common.log(f))
    } else {
      Common.log("\nMatching files: 0")
    }
  }

  private def printMatchingLines(results: Seq[SearchResult], settings: SearchSettings): Unit = {
    val lines = getMatchingLines(results, settings)
    val hdr =
      if (settings.uniqueLines) {
        "\nUnique matching lines"
      } else {
        "\nMatching lines"
      }
    if (lines.nonEmpty) {
      Common.log("%s (%d):".format(hdr, lines.length))
      lines.foreach(Common.log)
    } else {
      Common.log("%s: 0".format(hdr))
    }
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
        printSearchResults(results, searcher)
      }
      if (settings.printDirs) { printMatchingDirs(results) }
      if (settings.printFiles) { printMatchingFiles(results) }
      if (settings.printLines) { printMatchingLines(results, settings) }

    } catch {
      case e: SearchException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        SearchOptions.usage(1)
    }
  }
}
