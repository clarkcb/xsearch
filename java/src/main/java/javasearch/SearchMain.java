/*******************************************************************************
SearchMain

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public class SearchMain {

    private static void log(String message) {
        System.out.println(message);
    }

    private static void logError(String message) {
        log("Error: " + message);
    }

    public static void main(String[] args) {

		SearchOptions options = new SearchOptions();
		SearchSettings settings = new SearchSettings();

		try {
			settings = options.settingsFromArgs(args);
		} catch (SearchException e) {
            log("");
			logError(e.getMessage() + "\n");
			options.usage(1);
		}

		if (settings.getDebug()) {
			log("\nsettings:");
			log(settings.toString() + "\n");
		}

		if (settings.getPrintUsage()) {
			options.usage(0);
		}

		try {
            Searcher searcher = new Searcher(settings);
			searcher.search();

            // print the results
            if (settings.getPrintResults()) {
                log("");
                searcher.printSearchResults();
            }
            if (settings.getListDirs()) {
                searcher.printMatchingDirs();
            }
            if (settings.getListFiles()) {
                searcher.printMatchingFiles();
            }
            if (settings.getListLines()) {
                searcher.printMatchingLines();
            }

		} catch (SearchException e) {
            log("");
			logError(e.getMessage() + "\n");
			options.usage(1);
		}
    }
}
