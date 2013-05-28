package scalasearch

object SearchMain {

  def main(args: Array[String]) {
    if (args.length == 0) {
      println("Error: missing required arguments\n")
      SearchOptions.usage(1)
    }

    val arglist = args.toList

    var settings = new SearchSettings

    try {
     settings = SearchOptions.settingsFromArgs(arglist) 
    } catch {
      case ae: AssertionError => 
        println("Error: " + ae.getMessage + "\n")
        SearchOptions.usage(1)
      case e: Exception => 
        println("Error: " + e.getMessage + "\n")
        SearchOptions.usage(1)
    }

    if (settings.debug) {
      settings.verbose = true
      println("\nsettings:")
      println(settings.toString + "\n")
    }

    if (settings.printusage) {
      SearchOptions.usage(0)
    }

    // setting this as the default for when Searcher
    // is used via the command line
    settings.printresults = true

    val searcher = new Searcher(settings)
    searcher.search()
  }
}
