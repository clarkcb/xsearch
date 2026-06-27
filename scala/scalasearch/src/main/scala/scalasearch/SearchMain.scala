package scalasearch

import scalafind.{Common, Finder, FindException}

object SearchMain {
  def main(args: Array[String]): Unit = {
    var colorize = true
    try {
      val settings = SearchOptions.settingsFromArgs(args)
      colorize = settings.colorize

      if (settings.debug) {
        Common.log("settings: " + settings)
      }

      if (settings.printUsage) {
        Common.log("")
        SearchOptions.usage(0)
      }

      val searcher = new Searcher(settings)
      val results = searcher.search()
      val formatter = new SearchResultFormatter(settings)

      if (settings.printResults) {
        Searcher.printSearchResults(results, formatter)
      }
      if (settings.printDirs || settings.printFiles) {
        val fileResults = Searcher.getMatchingFileResults(results)
        if (settings.printDirs) {
          Finder.printMatchingDirs(fileResults, formatter.fileResultFormatter)
        }
        if (settings.printFiles) {
          Finder.printMatchingFiles(fileResults, formatter.fileResultFormatter)
        }
      }
      if (settings.printLines) { searcher.printMatchingLines(results, formatter) }
      if (settings.printMatches) { searcher.printMatches(results, formatter) }

    } catch {
      case e: FindException =>
        Common.log("")
        Common.logError(e.getMessage + "\n", colorize)
        SearchOptions.usage(1)
      case e: SearchException =>
        Common.log("")
        Common.logError(e.getMessage + "\n", colorize)
        SearchOptions.usage(1)
    }
  }
}
