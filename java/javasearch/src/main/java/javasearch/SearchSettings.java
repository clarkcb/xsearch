/*******************************************************************************
SearchSettings

Class to encapsulate search settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public class SearchSettings {

    private static final int INITIAL_SET_CAPACITY = 4;

    private boolean archivesOnly;
    private boolean colorize;
    private boolean debug;
    private boolean excludeHidden;
    private boolean firstMatch;
    private Set<String> inArchiveExtensions;
    private Set<Pattern> inArchiveFilePatterns;
    private Set<Pattern> inDirPatterns;
    private Set<String> inExtensions;
    private Set<Pattern> inFilePatterns;
    private Set<FileType> inFileTypes;
    private Set<Pattern> inLinesAfterPatterns;
    private Set<Pattern> inLinesBeforePatterns;
    private int linesAfter;
    private Set<Pattern> linesAfterToPatterns;
    private Set<Pattern> linesAfterUntilPatterns;
    private int linesBefore;
    private boolean listDirs;
    private boolean listFiles;
    private boolean listLines;
    private int maxLineLength;
    private boolean multiLineSearch;
    private Set<String> outArchiveExtensions;
    private Set<Pattern> outArchiveFilePatterns;
    private Set<Pattern> outDirPatterns;
    private Set<String> outExtensions;
    private Set<Pattern> outFilePatterns;
    private Set<FileType> outFileTypes;
    private Set<Pattern> outLinesAfterPatterns;
    private Set<Pattern> outLinesBeforePatterns;
    private boolean printResults;
    private boolean printUsage;
    private boolean printVersion;
    private boolean recursive;
    private boolean searchArchives;
    private Set<Pattern> searchPatterns;
    private String startPath;
    private String textFileEncoding;
    private boolean uniqueLines;
    private boolean verbose;

    public SearchSettings() {
        this.archivesOnly = DefaultSettings.ARCHIVESONLY;
        this.colorize = DefaultSettings.COLORIZE;
        this.debug = DefaultSettings.DEBUG;
        this.firstMatch = DefaultSettings.FIRSTMATCH;
        this.excludeHidden = DefaultSettings.EXCLUDEHIDDEN;
        this.inArchiveExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inArchiveFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inDirPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inFileTypes = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inLinesAfterPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inLinesBeforePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.linesAfter = DefaultSettings.LINESAFTER;
        this.linesAfterToPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.linesAfterUntilPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.linesBefore = DefaultSettings.LINESBEFORE;
        this.listDirs = DefaultSettings.LISTDIRS;
        this.listFiles = DefaultSettings.LISTFILES;
        this.listLines = DefaultSettings.LISTLINES;
        this.maxLineLength = DefaultSettings.MAXLINELENGTH;
        this.multiLineSearch = DefaultSettings.MULTILINESEARCH;
        this.outArchiveExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outArchiveFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outDirPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outFileTypes = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outLinesAfterPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outLinesBeforePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.printResults = DefaultSettings.PRINTRESULTS;
        this.printUsage = DefaultSettings.PRINTUSAGE;
        this.printVersion = DefaultSettings.PRINTVERSION;
        this.recursive = DefaultSettings.RECURSIVE;
        this.searchArchives = DefaultSettings.SEARCHARCHIVES;
        this.searchPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.textFileEncoding = DefaultSettings.TEXTFILEENCODING;
        this.uniqueLines = DefaultSettings.UNIQUELINES;
        this.verbose = DefaultSettings.VERBOSE;
    }

    public final String getStartPath() {
        return this.startPath;
    }

    public final void setStartPath(final String startPath) {
        this.startPath = startPath;
    }

    public final boolean getArchivesOnly() {
        return this.archivesOnly;
    }

    public final void setArchivesOnly(final boolean archivesOnly) {
        this.archivesOnly = archivesOnly;
        if (archivesOnly) {
            this.searchArchives = true;
        }
    }

    public final boolean getColorize() {
        return this.colorize;
    }

    public final void setColorize(final boolean colorize) {
        this.colorize = colorize;
    }

    public final boolean getDebug() {
        return this.debug;
    }

    public final void setDebug(final boolean debug) {
        this.debug = debug;
        if (debug) {
            this.verbose = true;
        }
    }

    public final boolean getFirstMatch() {
        return this.firstMatch;
    }

    public final void setFirstMatch(final boolean firstMatch) {
        this.firstMatch = firstMatch;
    }

    public final boolean getExcludeHidden() {
        return this.excludeHidden;
    }

    public final void setExcludeHidden(final boolean excludeHidden) {
        this.excludeHidden = excludeHidden;
    }

    public final int getLinesAfter() {
        return this.linesAfter;
    }

    public final void setLinesAfter(final int linesAfter) {
        this.linesAfter = linesAfter;
    }

    public final int getLinesBefore() {
        return this.linesBefore;
    }

    public final void setLinesBefore(final int linesBefore) {
        this.linesBefore = linesBefore;
    }

    public final boolean getListDirs() {
        return this.listDirs;
    }

    public final void setListDirs(final boolean listDirs) {
        this.listDirs = listDirs;
    }

    public final boolean getListFiles() {
        return this.listFiles;
    }

    public final void setListFiles(final boolean listFiles) {
        this.listFiles = listFiles;
    }

    public final boolean getListLines() {
        return this.listLines;
    }

    public final void setListLines(final boolean listLines) {
        this.listLines = listLines;
    }

    public final int getMaxLineLength() {
        return this.maxLineLength;
    }

    public final void setMaxLineLength(final int maxLineLength) {
        this.maxLineLength = maxLineLength;
    }

    public final boolean getMultiLineSearch() {
        return this.multiLineSearch;
    }

    public final void setMultiLineSearch(final boolean multiLineSearch) {
        this.multiLineSearch = multiLineSearch;
    }

    public final boolean getPrintResults() {
        return this.printResults;
    }

    public final void setPrintResults(final boolean printResults) {
        this.printResults = printResults;
    }

    public final boolean getPrintUsage() {
        return this.printUsage;
    }

    public final void setPrintUsage(final boolean printUsage) {
        this.printUsage = printUsage;
    }

    public final boolean getPrintVersion() {
        return this.printVersion;
    }

    public final void setPrintVersion(final boolean printVersion) {
        this.printVersion = printVersion;
    }

    public final boolean getRecursive() {
        return this.recursive;
    }

    public final void setRecursive(final boolean recursive) {
        this.recursive = recursive;
    }

    public final boolean getSearchArchives() {
        return this.searchArchives;
    }

    public final void setSearchArchives(final boolean searchArchives) {
        this.searchArchives = searchArchives;
    }

    public String getTextFileEncoding() {
        return textFileEncoding;
    }

    public void setTextFileEncoding(String textFileEncoding) {
        this.textFileEncoding = textFileEncoding;
    }

    public final boolean getUniqueLines() {
        return this.uniqueLines;
    }

    public final void setUniqueLines(final boolean uniqueLines) {
        this.uniqueLines = uniqueLines;
    }

    public final boolean getVerbose() {
        return this.verbose;
    }

    public final void setVerbose(final boolean verbose) {
        this.verbose = verbose;
    }

    // could be a comma-separated list
    private static void addExtensions(Set<String> set, final String exts) {
        addExtensions(set, Arrays.asList(exts.split(",")));
    }

    private static void addExtensions(Set<String> set, final List<String> exts) {
        for (String x : exts) {
            if (!x.isEmpty()) {
                set.add(x.toLowerCase());
            }
        }
    }

    public final Set<String> getInExtensions() {
        return this.inExtensions;
    }

    public final void addInExtension(final String ext) {
        addExtensions(this.inExtensions, ext);
    }

    public final Set<String> getOutExtensions() {
        return this.outExtensions;
    }

    public final void addOutExtension(final String ext) {
        addExtensions(this.outExtensions, ext);
    }

    public final Set<String> getInArchiveExtensions() {
        return this.inArchiveExtensions;
    }

    public final void addInArchiveExtension(final String ext) {
        addExtensions(this.inArchiveExtensions, ext);
    }

    public final Set<String> getOutArchiveExtensions() {
        return this.outArchiveExtensions;
    }

    public final void addOutArchiveExtension(final String ext) {
        addExtensions(this.outArchiveExtensions, ext);
    }

    private static void addPattern(Set<Pattern> set, final String pattern) {
        set.add(Pattern.compile(pattern));
    }

    public final Set<Pattern> getInDirPatterns() {
        return this.inDirPatterns;
    }

    public final void addInDirPattern(final String pattern) {
        addPattern(this.inDirPatterns, pattern);
    }

    public final Set<Pattern> getOutDirPatterns() {
        return this.outDirPatterns;
    }

    public final void addOutDirPattern(final String pattern) {
        addPattern(this.outDirPatterns, pattern);
    }

    public final Set<Pattern> getInFilePatterns() {
        return this.inFilePatterns;
    }

    public final void addInFilePattern(final String pattern) {
        addPattern(this.inFilePatterns, pattern);
    }

    public final Set<Pattern> getOutFilePatterns() {
        return this.outFilePatterns;
    }

    public final void addOutFilePattern(final String pattern) {
        addPattern(this.outFilePatterns, pattern);
    }

    public final Set<Pattern> getInArchiveFilePatterns() {
        return this.inArchiveFilePatterns;
    }

    public final void addInArchiveFilePattern(final String pattern) {
        addPattern(this.inArchiveFilePatterns, pattern);
    }

    public final Set<Pattern> getOutArchiveFilePatterns() {
        return this.outArchiveFilePatterns;
    }

    public final void addOutArchiveFilePattern(final String pattern) {
        addPattern(this.outArchiveFilePatterns, pattern);
    }

    public final Set<Pattern> getInLinesAfterPatterns() {
        return this.inLinesAfterPatterns;
    }

    public final void addInLinesAfterPattern(final String pattern) {
        addPattern(this.inLinesAfterPatterns, pattern);
    }

    public final Set<Pattern> getOutLinesAfterPatterns() {
        return this.outLinesAfterPatterns;
    }

    public final void addOutLinesAfterPattern(final String pattern) {
        addPattern(this.outLinesAfterPatterns, pattern);
    }

    public final Set<Pattern> getInLinesBeforePatterns() {
        return this.inLinesBeforePatterns;
    }

    public final void addInLinesBeforePattern(final String pattern) {
        addPattern(this.inLinesBeforePatterns, pattern);
    }

    public final Set<Pattern> getOutLinesBeforePatterns() {
        return this.outLinesBeforePatterns;
    }

    public final void addOutLinesBeforePattern(final String pattern) {
        addPattern(this.outLinesBeforePatterns, pattern);
    }

    public final Set<Pattern> getLinesAfterToPatterns() {
        return this.linesAfterToPatterns;
    }

    public final void addLinesAfterToPattern(final String pattern) {
        addPattern(this.linesAfterToPatterns, pattern);
    }


    public final Set<Pattern> getLinesAfterUntilPatterns() {
        return this.linesAfterUntilPatterns;
    }

    public final void addLinesAfterUntilPattern(final String pattern) {
        addPattern(this.linesAfterUntilPatterns, pattern);
    }

    public final void addSearchPattern(final String pattern) {
        addPattern(this.searchPatterns, pattern);
    }

    public final Set<Pattern> getSearchPatterns() {
        return this.searchPatterns;
    }

    public final boolean hasLinesAfterToPatterns() {
        return linesAfterToPatterns.size() > 0;
    }

    public final boolean hasLinesAfterUntilPatterns() {
        return linesAfterUntilPatterns.size() > 0;
    }

    public final boolean hasLinesAfterToOrUntilPatterns() {
        return hasLinesAfterToPatterns() || hasLinesAfterUntilPatterns();
    }

    // could be a comma-separated list
    private static void addEFileTypes(Set<FileType> set, final String fts) {
        addEFileTypes(set, Arrays.asList(fts.split(",")));
    }

    private static void addEFileTypes(Set<FileType> set, final List<String> fts) {
        for (String ft : fts) {
            if (!ft.isEmpty()) {
                set.add(FileTypes.fromName(ft));
            }
        }
    }

    public final void addInFileType(final String ft) {
        addEFileTypes(this.inFileTypes, ft);
    }

    public final void addOutFileType(final String ft) {
        addEFileTypes(this.outFileTypes, ft);
    }

    private static String stringSetToString(final Set<String> set) {
        StringBuilder sb = new StringBuilder("[");
        int elemCount = 0;
        for (String s : set) {
            if (elemCount > 0) {
                sb.append(", ");
            }
            sb.append("\"").append(s).append("\"");
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    private static String patternSetToString(final Set<Pattern> set) {
        StringBuilder sb = new StringBuilder("[");
        int elemCount = 0;
        for (Pattern p : set) {
            if (elemCount > 0) {
                sb.append(", ");
            }
            sb.append("\"").append(p.toString()).append("\"");
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    public final String toString() {
        String startPath;
        if (null == this.startPath) {
            startPath = "\"\"";
        } else {
            startPath = "\"" + this.startPath + "\"";
        }
        return "SearchSettings("
                + "archivesOnly: " + this.archivesOnly
                + ", colorize: " + this.colorize
                + ", debug: " + this.debug
                + ", excludeHidden: " + this.excludeHidden
                + ", firstMatch: " + this.firstMatch
                + ", inArchiveExtensions: " + stringSetToString(this.inArchiveExtensions)
                + ", inArchiveFilePatterns: " + patternSetToString(this.inArchiveFilePatterns)
                + ", inDirPatterns: " + patternSetToString(this.inDirPatterns)
                + ", inExtensions: " + stringSetToString(this.inExtensions)
                + ", inFilePatterns: " + patternSetToString(this.inFilePatterns)
                + ", inLinesAfterPatterns: " + patternSetToString(this.inLinesAfterPatterns)
                + ", inLinesBeforePatterns: " + patternSetToString(this.inLinesBeforePatterns)
                + ", linesAfter: " + this.linesAfter
                + ", linesAfterToPatterns: " + patternSetToString(this.linesAfterToPatterns)
                + ", linesAfterUntilPatterns: " + patternSetToString(this.linesAfterUntilPatterns)
                + ", linesBefore: " + this.linesBefore
                + ", listDirs: " + this.listDirs
                + ", listFiles: " + this.listFiles
                + ", listLines: " + this.listLines
                + ", maxLineLength: " + this.maxLineLength
                + ", multiLineSearch: " + this.multiLineSearch
                + ", outArchiveExtensions: " + stringSetToString(this.outArchiveExtensions)
                + ", outArchiveFilePatterns: " + patternSetToString(this.outArchiveFilePatterns)
                + ", outDirPatterns: " + patternSetToString(this.outDirPatterns)
                + ", outExtensions: " + stringSetToString(this.outExtensions)
                + ", outFilePatterns: " + patternSetToString(this.outFilePatterns)
                + ", outLinesAfterPatterns: " + patternSetToString(this.outLinesAfterPatterns)
                + ", outLinesBeforePatterns: " + patternSetToString(this.outLinesBeforePatterns)
                + ", printResults: " + this.printResults
                + ", printUsage: " + this.printUsage
                + ", printVersion: " + this.printVersion
                + ", recursive: " + this.recursive
                + ", searchArchives: " + this.searchArchives
                + ", searchPatterns: " + patternSetToString(this.searchPatterns)
                + ", startPath: " + startPath
                + ", textFileEncoding: " + textFileEncoding
                + ", uniqueLines: " + this.uniqueLines
                + ", verbose: " + this.verbose
                + ")";
    }
}
