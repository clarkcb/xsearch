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

    private static int signum(final int num) {
        return Integer.compare(num, 0);
    }

    private static int compareResults(final SearchResult r1, final SearchResult r2) {
        int pathCmp = r1.getSearchFile().getPath().toLowerCase()
                .compareTo(r2.getSearchFile().getPath().toLowerCase());
        if (pathCmp == 0) {
            int fileCmp = r1.getSearchFile().getFileName().toLowerCase()
                    .compareTo(r2.getSearchFile().getFileName().toLowerCase());
            if (fileCmp == 0) {
                int lineNumCmp = signum(r1.getLineNum() - r2.getLineNum());
                if (lineNumCmp == 0) {
                    return signum(r1.getMatchStartIndex() - r2.getMatchStartIndex());
                }
                return lineNumCmp;
            }
            return fileCmp;
        }
        return pathCmp;
    }

    private static List<SearchResult> getSortedSearchResults(List<SearchResult> results) {
        return results.stream().sorted(SearchMain::compareResults)
                .collect(Collectors.toList());
    }

    private static void printSearchResults(List<SearchResult> results, SearchSettings settings) {
        List<SearchResult> sortedResults = getSortedSearchResults(results);
        SearchResultFormatter formatter = new SearchResultFormatter(settings);
        log(String.format("Search results (%d):", sortedResults.size()));
        for (SearchResult r : sortedResults) {
            log(formatter.format(r));
        }
    }

    private static List<String> getMatchingDirs(List<SearchResult> results) {
        return results.stream().map(r -> r.getSearchFile().getPath()).distinct()
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
        return results.stream().map(r -> r.getSearchFile().toString()).distinct()
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
                    printSearchResults(results, settings);
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
