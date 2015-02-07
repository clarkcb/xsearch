/*******************************************************************************
SearchSettings

Class to encapsulate search settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

public class SearchSettings {

    private String startPath;

    private Set<String> inExtensions;
    private Set<String> outExtensions;
    private Set<Pattern> inDirPatterns;
    private Set<Pattern> outDirPatterns;
    private Set<Pattern> inFilePatterns;
    private Set<Pattern> outFilePatterns;
    private Set<String> inArchiveExtensions;
    private Set<String> outArchiveExtensions;
    private Set<Pattern> inArchiveFilePatterns;
    private Set<Pattern> outArchiveFilePatterns;
    private Set<Pattern> inLinesAfterPatterns;
    private Set<Pattern> outLinesAfterPatterns;
    private Set<Pattern> inLinesBeforePatterns;
    private Set<Pattern> outLinesBeforePatterns;
    private Set<Pattern> linesAfterToPatterns;
    private Set<Pattern> linesAfterUntilPatterns;
    private Set<Pattern> searchPatterns;

    private boolean archivesOnly;
    private boolean debug;
    private boolean doTiming;
    private boolean excludeHidden;
    private boolean firstMatch;
    private int linesAfter;
    private int linesBefore;
    private boolean listDirs;
    private boolean listFiles;
    private boolean listLines;
    private int maxLineLength;
    private boolean multiLineSearch;
    private boolean printResults;
    private boolean printUsage;
    private boolean printVersion;
    private boolean recursive;
    private boolean searchArchives;
    private boolean uniqueLines;
    private boolean verbose;

    public SearchSettings() {
        this.inExtensions = new HashSet<String>();
        this.outExtensions = new HashSet<String>();
        this.inDirPatterns = new HashSet<Pattern>();
        this.outDirPatterns = new HashSet<Pattern>();
        this.inFilePatterns = new HashSet<Pattern>();
        this.outFilePatterns = new HashSet<Pattern>();
        this.inArchiveExtensions = new HashSet<String>();
        this.outArchiveExtensions = new HashSet<String>();
        this.inArchiveFilePatterns = new HashSet<Pattern>();
        this.outArchiveFilePatterns = new HashSet<Pattern>();
        this.inLinesAfterPatterns = new HashSet<Pattern>();
        this.outLinesAfterPatterns = new HashSet<Pattern>();
        this.inLinesBeforePatterns = new HashSet<Pattern>();
        this.outLinesBeforePatterns = new HashSet<Pattern>();
        this.linesAfterToPatterns = new HashSet<Pattern>();
        this.linesAfterUntilPatterns = new HashSet<Pattern>();

        this.searchPatterns = new HashSet<Pattern>();

        // set the defaults
        this.archivesOnly = false;
        this.debug = false;
        this.doTiming = false;
        this.firstMatch = false;
        this.excludeHidden = true;
        this.linesAfter = 0;
        this.linesBefore = 0;
        this.listDirs = false;
        this.listFiles = false;
        this.listLines = false;
        this.maxLineLength = 150;
        this.multiLineSearch = false;
        this.printResults = true;
        this.printUsage = false;
        this.printVersion = false;
        this.recursive = true;
        this.searchArchives = false;
        this.uniqueLines = false;
        this.verbose = false;
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
        if (archivesOnly)
            this.searchArchives = true;
    }

    public final boolean getDebug() {
        return this.debug;
    }

    public final void setDebug(final boolean debug) {
        this.debug = debug;
        if (debug)
            this.verbose = true;
    }

    public final boolean getDoTiming() {
        return this.doTiming;
    }

    public final void setDoTiming(final boolean doTiming) {
        this.doTiming = doTiming;
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
        for (String x : exts.split(",")) {
            if (!x.equals(""))
                set.add(x.toLowerCase());
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

    public final void addSearchPattern(String pattern) {
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

    private static String stringSetToString(final Set<String> set) {
        StringBuilder sb = new StringBuilder("[");
        int elemCount = 0;
        for (String s : set) {
            if (elemCount > 0)
                sb.append(", ");
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
            if (elemCount > 0)
                sb.append(", ");
            sb.append("\"").append(p.toString()).append("\"");
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    public final String toString() {
        return "SearchSettings(" + "startPath: \"" + this.startPath + "\"" +
                ", inExtensions: " + stringSetToString(this.inExtensions) +
                ", outExtensions: " + stringSetToString(this.outExtensions) +
                ", inDirPatterns: " + patternSetToString(this.inDirPatterns) +
                ", outDirPatterns: " + patternSetToString(this.outDirPatterns) +
                ", inFilePatterns: " + patternSetToString(this.inFilePatterns) +
                ", outFilePatterns: " + patternSetToString(this.outFilePatterns) +
                ", searchPatterns: " + patternSetToString(this.searchPatterns) +
                ", inArchiveExtensions: " + stringSetToString(this.inArchiveExtensions) +
                ", outArchiveExtensions: " + stringSetToString(this.outArchiveExtensions) +
                ", inArchiveFilePatterns: " + patternSetToString(this.inArchiveFilePatterns) +
                ", outArchiveFilePatterns: " + patternSetToString(this.outArchiveFilePatterns) +
                ", archivesOnly: " + this.archivesOnly +
                ", debug: " + this.debug +
                ", doTiming: " + this.doTiming +
                ", excludeHidden: " + this.excludeHidden +
                ", firstMatch: " + this.firstMatch +
                ", linesAfter: " + this.linesAfter +
                ", linesBefore: " + this.linesBefore +
                ", listDirs: " + this.listDirs +
                ", listFiles: " + this.listFiles +
                ", listLines: " + this.listLines +
                ", maxLineLength: " + this.maxLineLength +
                ", multiLineSearch: " + this.multiLineSearch +
                ", printResults: " + this.printResults +
                ", printUsage: " + this.printUsage +
                ", printVersion: " + this.printVersion +
                ", recursive: " + this.recursive +
                ", searchArchives: " + this.searchArchives +
                ", uniqueLines: " + this.uniqueLines +
                ", verbose: " + this.verbose +
                ")";
    }
}
