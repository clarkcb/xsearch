/*******************************************************************************
Searcher

Class to perform the file search

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import org.apache.commons.io.LineIterator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Set;
import java.util.stream.Collectors;

public class Searcher {

    private SearchSettings settings;
    private List<SearchResult> results;
    private FileTypes fileTypes;
    private Set<SearchFile> searchFileSet;
    private Map<String, Long> timers;
    private long totalElapsedTime;

    public Searcher(final SearchSettings settings) {
        this.settings = settings;
        this.results = new ArrayList<>();
        this.fileTypes = new FileTypes();
        this.searchFileSet = new HashSet<>();
        this.timers = new HashMap<>();
        this.totalElapsedTime = 0L;
    }

    private void log(final String message) {
        System.out.println(message);
    }

    public final void validateSettings() throws SearchException {
        if (null == settings.getStartPath() || settings.getStartPath().equals("")) {
            throw new SearchException("Startpath not defined");
        }
        File startPathFile = new File(settings.getStartPath());
        if (!startPathFile.exists()) {
            throw new SearchException("Startpath not found");
        }
        if (settings.getSearchPatterns().isEmpty()) {
            throw new SearchException("No search patterns defined");
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

    public final boolean isSearchDir(final File d) {
        List<String> pathElems = FileUtil.splitPath(d.toString());
        if (settings.getExcludeHidden()
                && pathElems.stream().anyMatch(FileUtil::isHidden)) {
            return false;
        }
        return (settings.getInDirPatterns().isEmpty()
                ||
                anyMatchesAnyPattern(pathElems, settings.getInDirPatterns()))
                &&
                (settings.getOutDirPatterns().isEmpty()
                 ||
                 !anyMatchesAnyPattern(pathElems, settings.getOutDirPatterns()));
    }

    public final List<File> getSearchDirs(final File startPath) {
        List<File> searchDirs = new ArrayList<>();
        if (settings.getDebug()) {
            log(String.format("Getting files to search under %s",
                    startPath.getPath()));
        }
        searchDirs.add(startPath);
        if (settings.getRecursive()) {
            searchDirs.addAll(recGetSearchDirs(startPath));
        }
        return searchDirs;
    }

    private List<File> getSubDirs(final File dir) {
        List<File> subDirs = new ArrayList<>();
        File[] dirFiles = dir.listFiles();
        if (dirFiles != null) {
            for (File f : dirFiles) {
                if (f.isDirectory()) {
                    subDirs.add(f);
                }
            }
        }
        return subDirs;
    }

    private List<File> recGetSearchDirs(final File dir) {
        List<File> subDirs = getSubDirs(dir);
        List<File> searchDirs = subDirs.stream().filter(this::isSearchDir)
                .collect(Collectors.toList());
        for (File d : subDirs) {
            searchDirs.addAll(recGetSearchDirs(d));
        }
        return searchDirs;
    }

    public final boolean isSearchFile(final File f) {
        String ext = FileUtil.getExtension(f);
        return (settings.getInExtensions().isEmpty()
                ||
                settings.getInExtensions().contains(ext))
               &&
               (settings.getOutExtensions().isEmpty()
                ||
                !settings.getOutExtensions().contains(ext))
               &&
               (settings.getInFilePatterns().isEmpty()
                ||
                matchesAnyPattern(f.getName(), settings.getInFilePatterns()))
               &&
               (settings.getOutFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(f.getName(), settings.getOutFilePatterns()));
    }

    public final boolean isArchiveSearchFile(final File f) {
        String ext = FileUtil.getExtension(f);
        return (settings.getInArchiveExtensions().isEmpty()
                ||
                settings.getInArchiveExtensions().contains(ext))
               &&
               (settings.getOutArchiveExtensions().isEmpty()
                ||
                !settings.getOutArchiveExtensions().contains(ext))
               &&
               (settings.getInArchiveFilePatterns().isEmpty()
                ||
                matchesAnyPattern(f.getName(), settings.getInArchiveFilePatterns()))
               &&
               (settings.getOutArchiveFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(f.getName(), settings.getOutArchiveFilePatterns()));
    }

    public final boolean filterFile(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return false;
        }
        if (fileTypes.getFileType(f) == FileType.ARCHIVE) {
            return settings.getSearchArchives() && isArchiveSearchFile(f);
        }
        return !settings.getArchivesOnly() && isSearchFile(f);
    }

    private List<SearchFile> getSearchFilesForDir(final File dir) {
        if (settings.getDebug()) {
            log(String.format("Getting files to search under %s", dir));
        }
        List<SearchFile> searchFiles = new ArrayList<>();
        File[] currentFiles = dir.listFiles();
        if (currentFiles != null) {
            for (File f : currentFiles) {
                if (f.isFile()) {
                    FileType fileType = fileTypes.getFileType(f);
                    if (filterFile(f)) {
                        searchFiles.add(new SearchFile(f.getParent(), f.getName(),
                                fileType));
                    }
                }
            }
        }
        return searchFiles;
    }

    public final List<SearchFile> getSearchFiles(final List<File> searchDirs) {
        List<SearchFile> searchFiles = new ArrayList<>();
        for (File d : searchDirs) {
            searchFiles.addAll(getSearchFilesForDir(d));
        }
        return searchFiles;
    }

    private void addTimer(final String name, final String action) {
        timers.put(name + ":" + action, System.currentTimeMillis());
    }

    private void startTimer(final String name) {
        addTimer(name, "start");
    }

    private void stopTimer(final String name) {
        addTimer(name, "stop");
        totalElapsedTime += getElapsed(name);
    }

    public final long getElapsed(final String name) {
        long startTime = this.timers.get(name + ":start");
        long stopTime = this.timers.get(name + ":stop");
        return stopTime - startTime;
    }

    public final void printElapsed(final String name) {
        log(String.format("Elapsed time for \"%s\": %d ms",
                name, getElapsed(name)));
    }

    public final void printTotalElapsed() {
        log(String.format("Total elapsed time: %d ms", totalElapsedTime));
    }

    public final void search() throws SearchException {
        // figure out if startPath is a directory or a file and search accordingly
        File startPathFile = new File(settings.getStartPath());
        if (startPathFile.isDirectory()) {
            if (isSearchDir(startPathFile)) {
                searchPath(startPathFile);
            } else {
                throw new SearchException("Startpath does not match search settings");
            }
        } else if (startPathFile.isFile()) {
            FileType fileType = fileTypes.getFileType(startPathFile);
            if (filterFile(startPathFile)) {
                File d = startPathFile.getParentFile();
                if (null == d) {
                    d = new File(".");
                }
                searchFile(new SearchFile(d.getPath(), startPathFile.getName(),
                        fileType));
            } else {
                throw new SearchException("Startpath does not match search settings");
            }
        } else {
            throw new SearchException("Startpath is not a searchable file type");
        }
        if (settings.getVerbose()) {
            log("\nFile search complete.\n");
        }
    }

    public final void searchPath(final File filePath) {
        // get the search directories
        if (settings.getDoTiming()) {
            startTimer("getSearchDirs");
        }
        List<File> searchDirs = getSearchDirs(filePath);
        if (settings.getDoTiming()) {
            stopTimer("getSearchDirs");
            if (settings.getPrintResults()) {
                printElapsed("getSearchDirs");
            }
        }
        if (settings.getVerbose()) {
            log(String.format("\nDirectories to be searched (%d):",
                    searchDirs.size()));
            for (File d : searchDirs) {
                System.out.println(d.getPath());
            }
            log("");
        }

        // get the search files in the search directories
        if (settings.getDoTiming()) {
            startTimer("getSearchFiles");
        }
        List<SearchFile> searchFiles = getSearchFiles(searchDirs);
        if (settings.getDoTiming()) {
            stopTimer("getSearchFiles");
            if (settings.getPrintResults()) {
                printElapsed("getSearchFiles");
            }
        }
        if (settings.getVerbose()) {
            log(String.format("\nFiles to be searched (%d):",
                    searchFiles.size()));
            for (SearchFile sf : searchFiles) {
                log(sf.toString());
            }
            log("");
        }

        // search the files
        if (settings.getDoTiming()) {
            startTimer("searchFiles");
        }
        searchFiles.forEach(this::searchFile);
        if (settings.getDoTiming()) {
            stopTimer("searchFiles");
            if (settings.getPrintResults()) {
                printElapsed("searchFiles");
                printTotalElapsed();
            }
        }
    }

    public final void searchFile(final SearchFile sf) {
        FileType fileType = sf.getFileType();
        if (fileType == FileType.TEXT) {
            searchTextFile(sf);
        } else if (fileType == FileType.BINARY) {
            searchBinaryFile(sf);
        }
    }

    public final void searchTextFile(final SearchFile sf) {
        if (settings.getVerbose()) {
            log(String.format("Searching text file %s", sf.toString()));
        }
        if (settings.getMultiLineSearch()) {
            searchTextFileContents(sf);
        } else {
            searchTextFileLines(sf);
        }
    }

    public final void searchTextFileContents(final SearchFile sf) {
        try {
            String contents = FileUtil.getFileContents(sf.toFile());
            List<SearchResult> results = searchMultiLineString(contents);
            for (SearchResult r : results) {
                r.setSearchFile(sf);
                addSearchResult(r);
            }
        } catch (NoSuchElementException e) {
            log(e.toString() + ": " + sf.getPath());
        } catch (IllegalStateException | IOException e) {
            log(e.toString());
        }
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
                .map(i -> i.longValue() + 1).collect(Collectors.toList()));
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
            for (SearchResult r : searchMultiLineStringForPattern(s, p)) {
                results.add(r);
            }
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
                int endLineIndex = endLineIndices.get(beforeStartIndices.size() - 1).intValue();
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

    public final void searchTextFileLines(final SearchFile sf) {
        LineIterator it = null;
        try {
            it = FileUtil.getFileLineIterator(sf.toFile());
            List<SearchResult> results = searchStringIterator(it);
            for (SearchResult r : results) {
                r.setSearchFile(sf);
                addSearchResult(r);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (it != null) {
                it.close();
            }
        }
    }

    private boolean linesMatch(final List<String> lines,
                               final Set<Pattern> inPatterns,
                               final Set<Pattern> outPatterns) {
        return ((inPatterns.size() == 0 || anyMatchesAnyPattern(lines, inPatterns))
                &&
                (outPatterns.size() == 0 || !anyMatchesAnyPattern(lines, outPatterns)));
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

    public final void searchBinaryFile(final SearchFile sf) {
        if (settings.getVerbose()) {
            log(String.format("Searching binary file %s", sf.getPath()));
        }
        try {
            String content = FileUtil.getFileContents(sf.toFile(), "ISO8859-1");
            for (Pattern p : settings.getSearchPatterns()) {
                Matcher m = p.matcher(content);
                // TODO: find all matches and include start and end indices
                if (m.find()) {
                    addSearchResult(new SearchResult(p, sf, 0, 0, 0, ""));
                }
            }
        } catch (IOException | NoSuchElementException | IllegalStateException e) {
            log(e.toString());
        }
    }

    private void addSearchResult(final SearchResult searchResult) {
        results.add(searchResult);
        searchFileSet.add(searchResult.getSearchFile());
    }

    public final void printSearchResults() {
        log(String.format("Search results (%d):", results.size()));
        Collections.sort(results, (r1, r2) -> r1.getSearchFile().toFile()
                .getPath().compareTo(r2.getSearchFile().toFile().getPath()));
        for (SearchResult r : results) {
            log(r.toString());
        }
    }

    public final List<String> getMatchingDirs() {
        return searchFileSet.stream().map(SearchFile::getPath).sorted()
                .collect(Collectors.toList());
    }

    public final void printMatchingDirs() {
        List<String> dirs = getMatchingDirs();
        log(String.format("\nDirectories with matches (%d):", dirs.size()));
        for (String d : dirs) {
            log(d);
        }
    }

    public final List<String> getMatchingFiles() {
        return searchFileSet.stream().map(SearchFile::toString).sorted()
                .collect(Collectors.toList());
    }

    public final void printMatchingFiles() {
        List<String> files = getMatchingFiles();
        log(String.format("\nFiles with matches (%d):", files.size()));
        for (String f : files) {
            log(f);
        }
    }

    public final List<String> getMatchingLines() {
        List<String> lines = new ArrayList<>();
        for (SearchResult r : results) {
            lines.add(r.getLine().trim());
        }
        if (settings.getUniqueLines()) {
            Set<String> lineSet = new HashSet<>(lines);
            lines = new ArrayList<>(lineSet);
        }
        Collections.sort(lines, (s1, s2) -> s1.toUpperCase()
                .compareTo(s2.toUpperCase()));
        return lines;
    }

    public final void printMatchingLines() {
        List<String> lines = getMatchingLines();
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
}
