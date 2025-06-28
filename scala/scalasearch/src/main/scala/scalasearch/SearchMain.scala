package scalasearch

import scalafind.Common

object SearchMain {
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
      val formatter = new SearchResultFormatter(settings)

      if (settings.printResults) {
        searcher.printSearchResults(results, formatter)
      }
      if (settings.printDirs) { searcher.printMatchingDirs(results, formatter) }
      if (settings.printFiles) { searcher.printMatchingFiles(results, formatter) }
      if (settings.printLines) { searcher.printMatchingLines(results, formatter) }

    } catch {
      case e: SearchException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        SearchOptions.usage(1)
    }
  }
}
