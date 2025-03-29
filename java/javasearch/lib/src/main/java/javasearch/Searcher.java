/*******************************************************************************
Searcher

Class to perform the file search

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.*;
import org.apache.commons.io.LineIterator;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static javafind.Logger.log;

public class Searcher {

    final private SearchSettings settings;
    final private Finder finder;
    private Charset charset;

    public Searcher(final SearchSettings settings) {
        this.settings = settings;
        this.finder = new Finder(settings);
    }

    public SearchSettings getSettings() {
        return this.settings;
    }

    final void validateSettings() throws SearchException {
        try {
            this.finder.validateSettings();
        } catch (FindException e) {
            throw new SearchException(e.getMessage());
        }
        if (settings.getSearchPatterns().isEmpty()) {
            throw new SearchException("No search patterns defined");
        }
        if (settings.getLinesAfter() < 0) {
            throw new SearchException("Invalid linesafter");
        }
        if (settings.getLinesBefore() < 0) {
            throw new SearchException("Invalid linesbefore");
        }
        if (settings.getMaxLineLength() < 0) {
            throw new SearchException("Invalid maxlinelength");
        }
        try {
            charset = Charset.forName(settings.getTextFileEncoding());
        } catch (IllegalArgumentException e) {
            throw new SearchException("Invalid encoding provided");
        }
    }

    private boolean anyMatchesAnyPattern(final List<String> sList,
                                         final Set<Pattern> patternSet) {
        return sList.stream().anyMatch(s -> matchesAnyPattern(s, patternSet));
    }

    private boolean matchesAnyPattern(final String s,
                                      final Set<Pattern> patternSet) {
        return null != s && patternSet.stream().anyMatch(p -> p.matcher(s).find());
    }

    public final List<SearchResult> search() throws SearchException {
        List<FileResult> fileResults;
        try {
            fileResults = finder.find();
        } catch (FindException e) {
            throw new SearchException(e.getMessage());
        }

        if (settings.getVerbose()) {
            List<String> dirResults = fileResults.stream()
                    .map(fr -> fr.path().getParent())
                    .map(p -> p == null ? "." : p.toString())
                    .distinct()
                    .sorted(String::compareTo)
                    .toList();
            log(String.format("\nDirectories to be searched (%d):",
                    dirResults.size()));
            for (String d : dirResults) {
                log(d);
            }
            log(String.format("\n\nFiles to be searched (%d):",
                    fileResults.size()));
            for (FileResult fr : fileResults) {
                log(fr.toString());
            }
            log("");
        }

        List<SearchResult> searchResults = searchFiles(fileResults);

        if (settings.getVerbose()) {
            log("\nFile search complete.\n");
        }

        sortSearchResults(searchResults);
        return searchResults;
    }

    private List<SearchResult> searchFiles(List<FileResult> fileResults) {
        List<SearchResult> results = new ArrayList<>();

        int offset = 0;
        int batchSize = 1000;

        if (fileResults.size() > batchSize) {
            do {
                int toIndex = Math.min(offset + batchSize, fileResults.size());
                List<FileResult> nextBatch = fileResults.subList(offset, toIndex);
                results.addAll(batchSearchFiles(nextBatch));
                offset += batchSize;
            } while (offset < fileResults.size());
        } else {
            for (FileResult fr : fileResults) {
                results.addAll(searchFile(fr));
            }
        }

        return results;
    }

    private List<SearchResult> batchSearchFiles(List<FileResult> fileResults) {
        //System.out.println("Next batch: " + fileResults.size() + " searchFiles");
        List<SearchResult> results = new ArrayList<>();

        List<CompletableFuture<List<SearchResult>>> futures = new ArrayList<>(fileResults.size());
        for (FileResult fr : fileResults) {
            futures.add(CompletableFuture.supplyAsync(() -> searchFile(fr)));
        }
        for (CompletableFuture<List<SearchResult>> future : futures) {
            try {
                results.addAll(future.get());
            } catch (InterruptedException | ExecutionException e) {
                log(e.toString());
            }
        }
        return results;
    }

    public final List<SearchResult> searchFile(final FileResult fr) {
        FileType fileType = fr.fileType();
        if (fileType == FileType.CODE || fileType == FileType.TEXT || fileType == FileType.XML) {
            return searchTextFile(fr);
        } else if (fileType == FileType.BINARY) {
            return searchBinaryFile(fr);
        }
        return new ArrayList<>();
    }

    private List<SearchResult> searchTextFile(final FileResult fr) {
        if (settings.getVerbose()) {
            log(String.format("Searching text file %s", fr.toString()));
        }
        if (settings.getMultiLineSearch()) {
            return searchTextFileContents(fr);
        } else {
            return searchTextFileLines(fr);
        }
    }

    private List<SearchResult> searchTextFileContents(final FileResult fr) {
        List<SearchResult> results = new ArrayList<>();
        try {
            String contents = FileUtil.getFileContents(fr.path(), charset);
            results.addAll(searchMultiLineString(contents));
            for (SearchResult r : results) {
                r.setFileResult(fr);
            }
        } catch (NoSuchElementException e) {
            log(e.toString() + ": " + fr.toString());
        } catch (IllegalStateException | IOException e) {
            log(e.toString());
        }
        return results;
    }

    private List<Number> getNewLineIndices(final String s) {
        List<Number> newlineIndices = new ArrayList<>();
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\n') {
                newlineIndices.add(i);
            }
        }
        return newlineIndices;
    }

    private List<Number> getStartLineIndicesFromNewLineIndices(final List<Number> newLineIndices) {
        List<Number> startLineIndices = new ArrayList<>();
        startLineIndices.add(0);
        startLineIndices.addAll(newLineIndices.stream()
                .map(i -> i.longValue() + 1).toList());
        return startLineIndices;
    }

    private List<Number> getEndLineIndicesFromNewLineIndices(final String s,
                                                             final List<Number> newLineIndices) {
        List<Number> endLineIndices = new ArrayList<>(newLineIndices);
        endLineIndices.add(s.length() - 1);
        return endLineIndices;
    }

    public final List<SearchResult> searchMultiLineString(final String s) {
        List<SearchResult> results = new ArrayList<>();
        for (Pattern p : settings.getSearchPatterns()) {
            results.addAll(searchMultiLineStringForPattern(s, p));
        }
        return results;
    }

    private List<String> getLinesFromMultiLineString(final String s,
                                                     final List<Number> startIndices,
                                                     final List<Number> endIndices) {
        List<String> lines = new ArrayList<>();
        for (int i = 0; i < startIndices.size(); i++) {
            lines.add(s.substring(startIndices.get(i).intValue(),
                    endIndices.get(i).intValue()));
        }
        return lines;
    }

    private List<SearchResult> searchMultiLineStringForPattern(final String s,
                                                               final Pattern p) {
        Map<Pattern, Integer> patternMatches = new HashMap<>();
        List<SearchResult> results = new ArrayList<>();
        List<Number> newLineIndices = getNewLineIndices(s);
        List<Number> startLineIndices = getStartLineIndicesFromNewLineIndices(newLineIndices);
        List<Number> endLineIndices = getEndLineIndicesFromNewLineIndices(s,
                newLineIndices);
        final int linesBeforeCount = settings.getLinesBefore();
        final int linesAfterCount = settings.getLinesAfter();
        final int sLength = s.length();
        Matcher m = p.matcher(s);
        boolean found = m.find();
        while (found) {
            if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
                found = false;
            } else {
                // get the start line indices before the match index to get the current line number
                List<Number> beforeStartIndices = startLineIndices.stream()
                        .filter(i -> i.intValue() <= m.start())
                        .collect(Collectors.toList());
                int lineNum = beforeStartIndices.size();
                int nextEndLineIndex = endLineIndices.get(beforeStartIndices.size() - 1).intValue();
                if (nextEndLineIndex == sLength - 1) {
                    nextEndLineIndex++;
                }
                int endLineIndex = nextEndLineIndex;
                int startLineIndex = beforeStartIndices
                        .remove(beforeStartIndices.size() - 1).intValue();
                String line = s.substring(startLineIndex, endLineIndex);

                List<String> linesBefore;
                if (linesBeforeCount > 0) {
                    List<Number> beforeEndIndices = endLineIndices.stream()
                            .filter(i -> i.intValue() < m.start())
                            .collect(Collectors.toList());
                    List<Number> linesBeforeStartIndices = ListUtil
                            .takeRight(beforeStartIndices, linesBeforeCount);
                    List<Number> linesBeforeEndIndices = ListUtil
                            .takeRight(beforeEndIndices, linesBeforeCount);
                    linesBefore = getLinesFromMultiLineString(s,
                            linesBeforeStartIndices, linesBeforeEndIndices);
                } else {
                    linesBefore = new ArrayList<>();
                }

                List<String> linesAfter;
                if (linesAfterCount > 0) {
                    List<Number> afterStartIndices = startLineIndices.stream()
                            .filter(i -> i.intValue() > m.start())
                            .limit(linesAfterCount)
                            .collect(Collectors.toList());
                    List<Number> afterEndIndices = endLineIndices.stream()
                            .filter(i -> i.intValue() > endLineIndex)
                            .limit(linesAfterCount)
                            .collect(Collectors.toList());
                    linesAfter = getLinesFromMultiLineString(s,
                            afterStartIndices, afterEndIndices);
                } else {
                    linesAfter = new ArrayList<>();
                }

                if ((linesBefore.isEmpty() || linesBeforeMatch(linesBefore))
                        &&
                        (linesAfter.isEmpty() || linesAfterMatch(linesAfter))) {
                    SearchResult searchResult = new SearchResult(
                            p,
                            null,
                            lineNum,
                            m.start() - startLineIndex + 1,
                            m.end() - startLineIndex + 1,
                            line,
                            linesBefore,
                            linesAfter);
                    results.add(searchResult);
                    patternMatches.put(p, 1);
                }
                found = m.find(m.end());
            }
        }
        return results;
    }

    private List<SearchResult> searchTextFileLines(final FileResult fr) {
        LineIterator it = null;
        List<SearchResult> results = new ArrayList<>();
        try {
            it = FileUtil.getFileLineIterator(fr.path(), settings.getTextFileEncoding());
            results.addAll(searchStringIterator(it));
            for (SearchResult r : results) {
                r.setFileResult(fr);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (it != null) {
                try {
                    it.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return results;
    }

    private boolean linesMatch(final List<String> lines,
                               final Set<Pattern> inPatterns,
                               final Set<Pattern> outPatterns) {
        return ((inPatterns.isEmpty() || anyMatchesAnyPattern(lines, inPatterns))
                &&
                (outPatterns.isEmpty() || !anyMatchesAnyPattern(lines, outPatterns)));
    }

    private boolean linesBeforeMatch(final List<String> linesBefore) {
        return linesMatch(linesBefore, settings.getInLinesBeforePatterns(),
                settings.getOutLinesBeforePatterns());
    }

    private boolean linesAfterMatch(final List<String> linesAfter) {
        return linesMatch(linesAfter, settings.getInLinesAfterPatterns(),
                settings.getOutLinesAfterPatterns());
    }

    private boolean linesAfterToOrUntilMatch(final Iterator<String> it, List<String> linesAfter) {
        Set<Pattern> linesAfterPatterns;
        if (settings.hasLinesAfterToPatterns()) {
            linesAfterPatterns = settings.getLinesAfterToPatterns();
        } else if (settings.hasLinesAfterUntilPatterns()) {
            linesAfterPatterns = settings.getLinesAfterUntilPatterns();
        } else { // should never get here
            linesAfterPatterns = new HashSet<>();
        }
        boolean linesAfterMatch = anyMatchesAnyPattern(linesAfter, linesAfterPatterns);
        while (!linesAfterMatch && it.hasNext()) {
            String nextLine = it.next();
            linesAfter.add(nextLine);
            linesAfterMatch = matchesAnyPattern(nextLine, linesAfterPatterns);
        }
        if (linesAfterMatch) {
            if (settings.hasLinesAfterUntilPatterns()) {
                linesAfter.remove(linesAfter.size() - 1);
            }
        }
        return linesAfterMatch;
    }

    public final List<SearchResult> searchStringIterator(final Iterator<String> it) {
        int lineNum = 0;
        String line;
        final int linesBeforeCount = settings.getLinesBefore();
        final int linesAfterCount = settings.getLinesAfter();
        List<String> linesBefore = new ArrayList<>();
        List<String> linesAfter = new ArrayList<>();
        Set<Pattern> matchedPatterns = new HashSet<>();
        List<SearchResult> results = new ArrayList<>();
        while (true) {
            lineNum++;
            if (!linesAfter.isEmpty()) {
                line = linesAfter.remove(0);
            } else if (it.hasNext()) {
                line = it.next();
            } else {
                break;
            }
            if (linesAfterCount > 0) {
                while (linesAfter.size() < linesAfterCount && it.hasNext()) {
                    linesAfter.add(it.next());
                }
            }

            Set<Pattern> searchPatterns = settings.getSearchPatterns()
                    .stream().filter(p -> !matchedPatterns.contains(p))
                    .collect(Collectors.toSet());
            for (Pattern p : searchPatterns) {
                Matcher m = p.matcher(line);
                boolean found = m.find();
                while (found) {
                    if ((linesBefore.isEmpty() || linesBeforeMatch(linesBefore))
                            &&
                            (linesAfter.isEmpty() || linesAfterMatch(linesAfter))
                            &&
                            (!settings.hasLinesAfterToOrUntilPatterns() ||
                                    linesAfterToOrUntilMatch(it, linesAfter))
                            )
                    {
                        SearchResult searchResult = new SearchResult(
                                p,
                                null,
                                lineNum,
                                m.start() + 1,
                                m.end() + 1,
                                line,
                                new ArrayList<>(linesBefore),
                                new ArrayList<>(linesAfter));
                        results.add(searchResult);
                        if (settings.getFirstMatch()) {
                            matchedPatterns.add(p);
                            found = false;
                        } else {
                            found = m.find(m.end());
                        }
                    } else {
                        found = false;
                    }
                }
            }

            if (linesBeforeCount > 0) {
                if (linesBefore.size() == linesBeforeCount) {
                    linesBefore.remove(0);
                }
                if (linesBefore.size() < linesBeforeCount) {
                    linesBefore.add(line);
                }
            }
            if (settings.getFirstMatch() &&
                    matchedPatterns.size() == settings.getSearchPatterns().size()) {
                break;
            }
        }
        return results;
    }

    private List<SearchResult> searchBinaryFile(final FileResult fr) {
        if (settings.getVerbose()) {
            log(String.format("Searching binary file %s", fr.path()));
        }
        List<SearchResult> results = new ArrayList<>();
        try {
            String content = FileUtil.getFileContents(fr.path(), StandardCharsets.ISO_8859_1);
            for (Pattern p : settings.getSearchPatterns()) {
                Matcher m = p.matcher(content);
                boolean found = m.find();
                while (found) {
                    results.add(new SearchResult(
                            p,
                            fr,
                            0,
                            m.start() + 1,
                            m.end() + 1,
                            ""));
                    found = !settings.getFirstMatch() && m.find(m.end());
                }
            }
        } catch (IOException | NoSuchElementException | IllegalStateException e) {
            log(e.toString());
        }
        return results;
    }

    public final void sortSearchResults(List<SearchResult> searchResults) {
        if (settings.getSortBy().equals(SortBy.FILENAME)) {
            searchResults.sort((sr1, sr2) -> sr1.compareByName(sr2, settings.getSortCaseInsensitive()));
        } else if (settings.getSortBy().equals(SortBy.FILESIZE)) {
            searchResults.sort((sr1, sr2) -> sr1.compareBySize(sr2, settings.getSortCaseInsensitive()));
        } else if (settings.getSortBy().equals(SortBy.FILETYPE)) {
            searchResults.sort((sr1, sr2) -> sr1.compareByType(sr2, settings.getSortCaseInsensitive()));
        } else if (settings.getSortBy().equals(SortBy.LASTMOD)) {
            searchResults.sort((sr1, sr2) -> sr1.compareByLastMod(sr2, settings.getSortCaseInsensitive()));
        } else {
            searchResults.sort((sr1, sr2) -> sr1.compareByPath(sr2, settings.getSortCaseInsensitive()));
        }
        if (settings.getSortDescending()) {
            Collections.reverse(searchResults);
        }
    }
}
