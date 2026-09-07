package scalasearch

import scalafind.{Common, FindException, Finder}

object SearchMain {
  def main(args: Array[String]): Unit = {
    var colorize = true
    var searchOptions: Option[SearchOptions] = None

    try {
      val config = new SearchConfig()
      searchOptions = Some(new SearchOptions(config))
      val settings = searchOptions.get.settingsFromArgs(args)
      colorize = settings.colorize

      if (settings.debug) {
        Common.log("settings: " + settings)
      }

      if (settings.printUsage) {
        Common.log("")
        searchOptions.foreach(_.usage(0))
      }

      val searcher = new Searcher(config, settings)
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
        searchOptions.foreach(_.usage(1))
      case e: SearchException =>
        Common.log("")
        Common.logError(e.getMessage + "\n", colorize)
        searchOptions.foreach(_.usage(1))
    }
  }
}
