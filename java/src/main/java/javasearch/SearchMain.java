/*******************************************************************************
SearchMain

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public class SearchMain {

	public static void main(String[] args) {

		SearchOptions options = new SearchOptions();
		SearchSettings settings = new SearchSettings();

		if (args.length < 1) {
			System.out.println("Error: missing required arguments\n");
			options.usage(1);
		}

		try {
			settings = options.settingsFromArgs(args);
		} catch (SearchException e) {
			System.out.println("Error: " + e.getMessage() + "\n");
			options.usage(1);
		}

		if (settings.getDebug()) {
			settings.setVerbose(true);
			System.out.println("\nsettings:");
			System.out.println(settings.toString() + "\n");
		}

		if (settings.getPrintUsage()) {
			options.usage(0);
		}

		Searcher searcher = new Searcher(settings);

		try {
			searcher.search();
		} catch (SearchException e) {
			System.out.println("Error: " + e.getMessage() + "\n");
			options.usage(1);
		}

		// print the results
        if (settings.getPrintResults()) {
			System.out.println();
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
    }
}
