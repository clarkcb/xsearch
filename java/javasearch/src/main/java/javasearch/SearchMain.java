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
import java.util.*;
import java.util.stream.Collectors;

import static javasearch.Logger.log;

public class SearchMain {

    private static void handleError(final String message) {
        log("");
        Logger.logError(message);
    }

    private static void handleError(final String message, SearchOptions options) {
        log("");
        Logger.logError(message + "\n");
        options.usage(1);
    }

    private static List<SearchResult> getSortedSearchResults(List<SearchResult> results, Searcher searcher) {
        return results.stream().sorted(searcher::compareResults)
                .collect(Collectors.toList());
    }

    private static void printSearchResults(List<SearchResult> results, Searcher searcher) {
        List<SearchResult> sortedResults = getSortedSearchResults(results, searcher);
        SearchResultFormatter formatter = new SearchResultFormatter(searcher.getSettings());
        log(String.format("Search results (%d):", sortedResults.size()));
        for (SearchResult r : sortedResults) {
            log(formatter.format(r));
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
        log(String.format("\nDirectories with matches (%d):", dirs.size()));
        for (String d : dirs) {
            log(d);
        }
    }

    private static List<String> getMatchingFiles(List<SearchResult> results) {
        return results.stream().map(r -> r.getFileResult().toString()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingFiles(List<SearchResult> results) {
        List<String> files = getMatchingFiles(results);
        log(String.format("\nFiles with matches (%d):", files.size()));
        for (String f : files) {
            log(f);
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
        log(String.format(hdr, lines.size()));
        for (String line : lines) {
            log(line);
        }
    }

    public static void main(final String[] args) {

        try {
            SearchOptions options = new SearchOptions();

            try {
                SearchSettings settings = options.settingsFromArgs(args);

                if (settings.getDebug()) {
                    log("\nsettings:");
                    log(settings.toString() + "\n");
                }

                if (settings.getPrintUsage()) {
                    log("");
                    options.usage(0);
                }

                Searcher searcher = new Searcher(settings);

                searcher.validateSettings();
                List<SearchResult> results = searcher.search();

                // print the results
                if (settings.getPrintResults()) {
                    log("");
                    printSearchResults(results, searcher);
                }
                if (settings.getListDirs()) {
                    printMatchingDirs(results);
                }
                if (settings.getListFiles()) {
                    printMatchingFiles(results);
                }
                if (settings.getListLines()) {
                    printMatchingLines(results, settings);
                }

            } catch (SearchException e) {
                handleError(e.getMessage(), options);
            }

        } catch (ParseException | IOException e) {
            handleError(e.getMessage());
        }
    }
}
