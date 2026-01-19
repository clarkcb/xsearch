/*******************************************************************************
JavaSearch

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.IOException;

import static javafind.Logger.log;
import static javafind.Logger.logError;


public class JavaSearch {

    private static void handleError(final String message, final boolean colorize) {
        log("");
        logError(message, colorize);
    }

    private static void handleError(final String message, final boolean colorize, SearchOptions options) {
        log("");
        logError(message + "\n",  colorize);
        options.usage(1);
    }

    public static void main(final String[] args) {
        var colorize = true;
        try {
            var options = new SearchOptions();

            try {
                var settings = options.settingsFromArgs(args);
                colorize = settings.getColorize();

                if (settings.getDebug()) {
                    log("\nsettings:");
                    log(settings.toString() + "\n");
                }

                if (settings.getPrintUsage()) {
                    log("");
                    options.usage(0);
                }

                var searcher = new Searcher(settings);
                searcher.validateSettings();
                var searchResults = searcher.search();
                var formatter = new SearchResultFormatter(settings);

                if (settings.getPrintResults()) {
                    log("");
                    Searcher.printSearchResults(searchResults, formatter);
                }
                if (settings.getPrintDirs()) {
                    Searcher.printMatchingDirs(searchResults, formatter);
                }
                if (settings.getPrintFiles()) {
                    Searcher.printMatchingFiles(searchResults, formatter);
                }
                if (settings.getPrintLines()) {
                    Searcher.printMatchingLines(searchResults, formatter);
                }
                if (settings.getPrintMatches()) {
                    Searcher.printMatches(searchResults, formatter);
                }

            } catch (SearchException e) {
                handleError(e.getMessage(), colorize, options);
            }

        } catch (IOException e) {
            handleError(e.getMessage(), colorize);
        }
    }
}
