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
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Searcher {

    final private SearchSettings settings;
    final private FileTypes fileTypes;
    private Charset charset;

    public Searcher(final SearchSettings settings) {
        this.settings = settings;
        this.fileTypes = new FileTypes();
    }

    private void log(final String message) {
        System.out.println(message);
    }

    final void validateSettings() throws SearchException {
        String startPath = settings.getStartPath();
        if (null == startPath || startPath.isEmpty()) {
            throw new SearchException("Startpath not defined");
        }
        File startPathFile = new File(startPath);
        if (!startPathFile.exists()) {
            throw new SearchException("Startpath not found");
        }
        if (!startPathFile.canRead()) {
            throw new SearchException("Startpath not readable");
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

    boolean isSearchDir(final File d) {
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

    // this is temporary to appease the tests
    boolean isSearchFile(final File f) {
        SearchFile sf = new SearchFile(f.getParent(), f.getName(), fileTypes.getFileType(f));
        return isSearchFile(sf);
    }

    boolean isSearchFile(final SearchFile sf) {
        String ext = FileUtil.getExtension(sf.getFileName());
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
                matchesAnyPattern(sf.getFileName(), settings.getInFilePatterns()))
               &&
               (settings.getOutFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(sf.getFileName(), settings.getOutFilePatterns()))
               &&
               (settings.getInFileTypes().isEmpty()
                ||
                settings.getInFileTypes().contains(sf.getFileType()))
               &&
               (settings.getOutFileTypes().isEmpty()
                ||
                settings.getOutFileTypes().contains(sf.getFileType()));
    }

    boolean isArchiveSearchFile(final File f) {
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

    boolean filterFile(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return false;
        }
        if (fileTypes.getFileType(f) == FileType.ARCHIVE) {
            return settings.getSearchArchives() && isArchiveSearchFile(f);
        }
        return !settings.getArchivesOnly() && isSearchFile(f);
    }

    SearchFile filterToSearchFile(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return null;
        }
        FileType fileType = fileTypes.getFileType(f);
        SearchFile searchFile = new SearchFile(f.getParent(), f.getName(), fileType);
        if ((fileType == FileType.ARCHIVE
                && (settings.getSearchArchives() || settings.getArchivesOnly())
                && isArchiveSearchFile(f))
                || (!settings.getArchivesOnly() && isSearchFile(searchFile))) {
            return searchFile;
        }
        return null;
    }

    public final List<SearchResult> search() throws SearchException {
        // figure out if startPath is a directory or a file and search accordingly
        List<SearchResult> results = new ArrayList<>();
        File startPathFile = new File(settings.getStartPath());
        if (startPathFile.isDirectory()) {
            if (isSearchDir(startPathFile)) {
                results.addAll(searchPath(startPathFile));
            } else {
                throw new SearchException("Startpath does not match search settings");
            }
        } else if (startPathFile.isFile()) {
            SearchFile searchFile = filterToSearchFile(startPathFile);
            if (searchFile != null) {
                results.addAll(searchFile(searchFile));
            } else {
                throw new SearchException("Startpath does not match search settings");
            }
        } else {
            throw new SearchException("Startpath is not a searchable file type");
        }
        if (settings.getVerbose()) {
            log("\nFile search complete.\n");
        }
        return results;
    }

    private static class FindSearchFileVisitor extends SimpleFileVisitor<Path> {
        FileTypes fileTypes;
        Function<File, Boolean> filterDir;
        Function<File, SearchFile> filterToSearchFile;
        List<SearchFile> searchFiles;

        FindSearchFileVisitor(FileTypes fileTypes, Function<File, Boolean> filterDir,
                              Function<File, SearchFile> filterToSearchFile) {
            super();
            this.fileTypes = fileTypes;
            this.filterDir = filterDir;
            this.filterToSearchFile = filterToSearchFile;
            searchFiles = new ArrayList<>();
        }

        @Override
        public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
            Objects.requireNonNull(dir);
            Objects.requireNonNull(attrs);
            if (filterDir.apply(dir.toFile())) {
                return FileVisitResult.CONTINUE;
            }
            return FileVisitResult.SKIP_SUBTREE;
        }

        @Override
        public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
            Objects.requireNonNull(path);
            Objects.requireNonNull(attrs);
            if (attrs.isRegularFile()) {
                File f = path.toFile();
                SearchFile sf = filterToSearchFile.apply(f);
                if (sf != null) {
                    searchFiles.add(sf);
                }
            }
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult visitFileFailed(Path path, IOException exc) {
            System.err.println(exc.getMessage());
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
            if (exc != null)
                throw exc;
            return FileVisitResult.CONTINUE;
        }
    }

    private List<SearchResult> searchPath(final File filePath) {
        Path startPath = filePath.toPath();
        FindSearchFileVisitor findSearchFileVisitor = new FindSearchFileVisitor(fileTypes,
                this::isSearchDir, this::filterToSearchFile);

        // walk file tree to get search files
        try {
            Files.walkFileTree(startPath, findSearchFileVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<SearchFile> searchFiles = findSearchFileVisitor.searchFiles;

        if (settings.getVerbose()) {
            List<String> searchDirs = searchFiles.stream()
                    .map(SearchFile::getPath)
                    .distinct()
                    .sorted(String::compareTo)
                    .collect(Collectors.toList());
            log(String.format("\nDirectories to be searched (%d):",
                    searchDirs.size()));
            for (String d : searchDirs) {
                log(d);
            }
            log(String.format("\n\nFiles to be searched (%d):",
                    searchFiles.size()));
            for (SearchFile sf : searchFiles) {
                log(sf.toString());
            }
            log("");
        }

        // search the files
        return searchFiles(searchFiles);
    }

    private List<SearchResult> searchFiles(List<SearchFile> searchFiles) {
        List<SearchResult> results = new ArrayList<>();

        int offset = 0;
        int batchSize = 1000;

        if (searchFiles.size() > batchSize) {
            do {
                int toIndex = Math.min(offset + batchSize, searchFiles.size());
                List<SearchFile> nextBatch = searchFiles.subList(offset, toIndex);
                results.addAll(batchSearchFiles(nextBatch));
                offset += batchSize;
            } while (offset < searchFiles.size());
        } else {
            for (SearchFile sf : searchFiles) {
                results.addAll(searchFile(sf));
            }
        }

        return results;
    }

    private List<SearchResult> batchSearchFiles(List<SearchFile> searchFiles) {
        //System.out.println("Next batch: " + searchFiles.size() + " searchFiles");
        List<SearchResult> results = new ArrayList<>();

        List<CompletableFuture<List<SearchResult>>> futures = new ArrayList<>(searchFiles.size());
        for (SearchFile sf : searchFiles) {
            futures.add(CompletableFuture.supplyAsync(() -> searchFile(sf)));
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

    public final List<SearchResult> searchFile(final SearchFile sf) {
        FileType fileType = sf.getFileType();
        if (fileType == FileType.CODE || fileType == FileType.TEXT || fileType == FileType.XML) {
            return searchTextFile(sf);
        } else if (fileType == FileType.BINARY) {
            return searchBinaryFile(sf);
        }
        return new ArrayList<>();
    }

    private List<SearchResult> searchTextFile(final SearchFile sf) {
        if (settings.getVerbose()) {
            log(String.format("Searching text file %s", sf.toString()));
        }
        if (settings.getMultiLineSearch()) {
            return searchTextFileContents(sf);
        } else {
            return searchTextFileLines(sf);
        }
    }

    private List<SearchResult> searchTextFileContents(final SearchFile sf) {
        List<SearchResult> results = new ArrayList<>();
        try {
            String contents = FileUtil.getFileContents(sf.toFile(), charset);
            results.addAll(searchMultiLineString(contents));
            for (SearchResult r : results) {
                r.setSearchFile(sf);
                //addSearchResult(r);
            }
        } catch (NoSuchElementException e) {
            log(e.toString() + ": " + sf.toString());
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

    private List<SearchResult> searchTextFileLines(final SearchFile sf) {
        LineIterator it = null;
        List<SearchResult> results = new ArrayList<>();
        try {
            it = FileUtil.getFileLineIterator(sf.toFile(), settings.getTextFileEncoding());
            results.addAll(searchStringIterator(it));
            for (SearchResult r : results) {
                r.setSearchFile(sf);
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

    private List<SearchResult> searchBinaryFile(final SearchFile sf) {
        if (settings.getVerbose()) {
            log(String.format("Searching binary file %s", sf.getPath()));
        }
        List<SearchResult> results = new ArrayList<>();
        try {
            String content = FileUtil.getFileContents(sf.toFile(), StandardCharsets.ISO_8859_1);
            for (Pattern p : settings.getSearchPatterns()) {
                Matcher m = p.matcher(content);
                boolean found = m.find();
                while (found) {
                    results.add(new SearchResult(
                            p,
                            sf,
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
}
