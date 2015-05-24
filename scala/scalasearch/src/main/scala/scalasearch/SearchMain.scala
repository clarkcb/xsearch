package scalasearch

object SearchMain {

  def printMatchingDirs(searcher:Searcher): Unit = {
    val dirs = searcher.getMatchingDirs
    Common.log("\nDirectories with matches (%d):".format(dirs.length))
    dirs.foreach(f => Common.log(f.toString))
  }

  def printMatchingFiles(searcher:Searcher): Unit = {
    val files = searcher.getMatchingFiles
    Common.log("\nFiles with matches (%d):".format(files.length))
    files.foreach(f => Common.log(f.toString))
  }

  def printMatchingLines(searcher:Searcher, settings:SearchSettings): Unit = {
    val lines = searcher.getMatchingLines
    val hdr =
      if (settings.uniqueLines) {
        "\nUnique lines with matches (%d):"
      } else {
        "\nLines with matches (%d):"
      }
    Common.log(hdr.format(lines.length))
    lines.foreach(Common.log)
  }

  def main(args: Array[String]) {
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
      searcher.search()

      if (settings.printResults) {
        Common.log("\nSearch results (%d):".format(searcher.searchResults.length))
        searcher.printSearchResults()
      }
      if (settings.listDirs) { printMatchingDirs(searcher) }
      if (settings.listFiles) { printMatchingFiles(searcher) }
      if (settings.listLines) { printMatchingLines(searcher, settings) }

    } catch {
      case e: SearchException =>
        Common.log("")
        Common.logError(e.getMessage + "\n")
        SearchOptions.usage(1)
    }
  }
}
