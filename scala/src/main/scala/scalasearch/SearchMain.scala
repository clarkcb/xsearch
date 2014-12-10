package scalasearch

object SearchMain {

  def main(args: Array[String]) {
    if (args.length == 0) {
      println("Error: missing required arguments\n")
      SearchOptions.usage(1)
    }

    val arglist = args.toList

    val settings =
      try {
        SearchOptions.settingsFromArgs(arglist)
      } catch {
        case ae: AssertionError =>
          println("Error: " + ae.getMessage + "\n")
          SearchOptions.usage(1)
        case e: Exception =>
          println("Error: " + e.getMessage + "\n")
          SearchOptions.usage(1)
      }

    if (settings.printUsage) {
      SearchOptions.usage(0)
    }

    val searcher = new Searcher(settings)
    searcher.search()

    if (settings.printResults) {
      println("Search results (%d):".format(searcher.searchResults.length))
      searcher.printSearchResults()
    }
    if (settings.listDirs) searcher.printMatchingDirs()
    if (settings.listFiles) searcher.printMatchingFiles()
    if (settings.listLines) searcher.printMatchingLines()
  }
}
