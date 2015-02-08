package scalasearch

object SearchMain {

  def log(message: String): Unit = {
    println(message)
  }

  def logError(message: String): Unit = {
    log("Error: " + message)
  }

  def main(args: Array[String]) {
    val settings =
      try {
        SearchOptions.settingsFromArgs(args)
      } catch {
        case e: SearchException =>
          log("")
          logError(e.getMessage + "\n")
          SearchOptions.usage(1)
      }

    if (settings.printUsage) {
      SearchOptions.usage(0)
    }

    try {
      val searcher = new Searcher(settings)
      searcher.search()

      if (settings.printResults) {
        log("\nSearch results (%d):".format(searcher.searchResults.length))
        searcher.printSearchResults()
      }
      if (settings.listDirs) searcher.printMatchingDirs()
      if (settings.listFiles) searcher.printMatchingFiles()
      if (settings.listLines) searcher.printMatchingLines()

    } catch {
      case e: SearchException =>
        log("")
        logError(e.getMessage + "\n")
        SearchOptions.usage(1)
    }
  }
}
