/*******************************************************************************
JavaSearch

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.FileResult;
import javafind.Finder;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static javafind.Logger.log;
import static javafind.Logger.logError;


public class JavaSearch {

    private static void handleError(final String message) {
        log("");
        logError(message);
    }

    private static void handleError(final String message, SearchOptions options) {
        log("");
        logError(message + "\n");
        options.usage(1);
    }

    public static void main(final String[] args) {
        try {
            var options = new SearchOptions();

            try {
                var settings = options.settingsFromArgs(args);

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

            } catch (SearchException e) {
                handleError(e.getMessage(), options);
            }

        } catch (IOException e) {
            handleError(e.getMessage());
        }
    }
}
