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

import javafind.Logger;

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

    private static void printSearchResults(List<SearchResult> results, SearchSettings settings) {
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        Logger.log(String.format("Search results (%d):", results.size()));
        for (SearchResult r : results) {
            Logger.log(formatter.format(r));
        }
    }

    private static List<String> getMatchingDirs(List<SearchResult> results) {
        return results.stream().map(r -> r.getFileResult().getPath().getParent())
                .map(p -> p == null ? "." : p.toString())
                .distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingDirs(List<SearchResult> results) {
        List<String> dirs = getMatchingDirs(results);
        Logger.log(String.format("\nDirectories with matches (%d):", dirs.size()));
        for (String d : dirs) {
            Logger.log(d);
        }
    }

    private static List<String> getMatchingFiles(List<SearchResult> results) {
        return results.stream().map(r -> r.getFileResult().toString()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingFiles(List<SearchResult> results) {
        List<String> files = getMatchingFiles(results);
        Logger.log(String.format("\nFiles with matches (%d):", files.size()));
        for (String f : files) {
            Logger.log(f);
        }
    }

    private static List<String> getMatchingLines(List<SearchResult> results, SearchSettings settings) {
        List<String> lines = new ArrayList<>();
        for (SearchResult r : results) {
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
        List<String> lines = getMatchingLines(results, settings);
        String hdr;
        if (settings.getUniqueLines()) {
            hdr = "\nUnique lines with matches (%d):";
        } else {
            hdr = "\nLines with matches (%d):";
        }
        Logger.log(String.format(hdr, lines.size()));
        for (String line : lines) {
            Logger.log(line);
        }
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
                    printSearchResults(results, settings);
                }
                if (settings.getPrintDirs()) {
                    printMatchingDirs(results);
                }
                if (settings.getPrintFiles()) {
                    printMatchingFiles(results);
                }
                if (settings.getPrintLines()) {
                    printMatchingLines(results, settings);
                }

            } catch (SearchException e) {
                handleError(e.getMessage(), options);
            }

        } catch (IOException e) {
            handleError(e.getMessage());
        }
    }
}
