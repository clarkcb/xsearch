/*******************************************************************************
SearchMain

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import org.json.simple.parser.ParseException;

import java.io.IOException;
import java.util.List;

public class SearchMain {

    private static void handleError(final String message) {
        Logger.log("");
        Logger.logError(message);
    }

    private static void handleError(final String message, SearchOptions options) {
        Logger.log("");
        Logger.logError(message + "\n");
        options.usage(1);
    }

    public static void main(final String[] args) {

        try {
            SearchOptions options = new SearchOptions();

            try {
                SearchSettings settings = options.settingsFromArgs(args);

                if (settings.getDebug()) {
                    Logger.log("\nsettings:");
                    Logger.log(settings.toString() + "\n");
                }

                if (settings.getPrintUsage()) {
                    Logger.log("");
                    options.usage(0);
                }

                Searcher searcher = new Searcher(settings);

                searcher.validateSettings();
                List<SearchResult> results = searcher.search();

                // print the results
                if (settings.getPrintResults()) {
                    Logger.log("");
                    searcher.printSearchResults(results);
                }
                if (settings.getListDirs()) {
                    searcher.printMatchingDirs(results);
                }
                if (settings.getListFiles()) {
                    searcher.printMatchingFiles(results);
                }
                if (settings.getListLines()) {
                    searcher.printMatchingLines(results);
                }

            } catch (SearchException e) {
                handleError(e.getMessage(), options);
            }

        } catch (ParseException | IOException e) {
            handleError(e.getMessage());
        }
    }
}
