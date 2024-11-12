/*******************************************************************************
SearchMain

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static javafind.Logger.log;
import static javafind.Logger.logError;

public class SearchMain {

    private static void handleError(final String message) {
        log("");
        logError(message);
    }

    private static void handleError(final String message, SearchOptions options) {
        log("");
        logError(message + "\n");
        options.usage(1);
    }

    private static void printSearchResults(List<SearchResult> results, SearchSettings settings) {
        var formatter = new SearchResultFormatter(settings);
        log(String.format("Search results (%d):", results.size()));
        for (var r : results) {
            log(formatter.format(r));
        }
    }

    private static List<String> getMatchingDirs(List<SearchResult> results) {
        return results.stream()
                .map(r -> r.getFileResult().getPath().getParent())
                .map(p -> p == null ? "." : p.toString())
                .distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingDirs(List<SearchResult> results) {
        var dirs = getMatchingDirs(results);
        log(String.format("\nDirectories with matches (%d):", dirs.size()));
        for (var d : dirs) {
            log(d);
        }
    }

    private static List<String> getMatchingFiles(List<SearchResult> results) {
        return results.stream()
                .map(r -> r.getFileResult().toString()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingFiles(List<SearchResult> results) {
        var files = getMatchingFiles(results);
        log(String.format("\nFiles with matches (%d):", files.size()));
        for (var f : files) {
            log(f);
        }
    }

    private static List<String> getMatchingLines(List<SearchResult> results, SearchSettings settings) {
        var lines = new ArrayList<String>();
        for (var r : results) {
            lines.add(r.getLine().trim());
        }
        if (settings.getUniqueLines()) {
            Set<String> lineSet = new HashSet<>(lines);
            lines = new ArrayList<>(lineSet);
        }
        lines.sort(Comparator.comparing(String::toUpperCase));
        return lines;
    }

    private static void printMatchingLines(List<SearchResult> results, SearchSettings settings) {
        var lines = getMatchingLines(results, settings);
        String hdr;
        if (settings.getUniqueLines()) {
            hdr = "\nUnique lines with matches (%d):";
        } else {
            hdr = "\nLines with matches (%d):";
        }
        log(String.format(hdr, lines.size()));
        for (var line : lines) {
            log(line);
        }
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

                if (settings.getPrintResults()) {
                    log("");
                    printSearchResults(searchResults, settings);
                }
                if (settings.getPrintDirs()) {
                    printMatchingDirs(searchResults);
                }
                if (settings.getPrintFiles()) {
                    printMatchingFiles(searchResults);
                }
                if (settings.getPrintLines()) {
                    printMatchingLines(searchResults, settings);
                }

            } catch (SearchException e) {
                handleError(e.getMessage(), options);
            }

        } catch (IOException e) {
            handleError(e.getMessage());
        }
    }
}
