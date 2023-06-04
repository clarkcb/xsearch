/*******************************************************************************
SearchSettings

Class to encapsulate search settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.FindSettings;
import javafind.SortByUtil;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

public class SearchSettings extends FindSettings {

    private static final int INITIAL_SET_CAPACITY = 4;

    private boolean colorize;
    private boolean firstMatch;
    private final Set<Pattern> inLinesAfterPatterns;
    private final Set<Pattern> inLinesBeforePatterns;
    private int linesAfter;
    private final Set<Pattern> linesAfterToPatterns;
    private final Set<Pattern> linesAfterUntilPatterns;
    private int linesBefore;
    private boolean listLines;
    private int maxLineLength;
    private boolean multiLineSearch;
    private final Set<Pattern> outLinesAfterPatterns;
    private final Set<Pattern> outLinesBeforePatterns;
    private boolean printResults;
    private boolean searchArchives;
    private final Set<Pattern> searchPatterns;
    private String textFileEncoding;
    private boolean uniqueLines;

    public SearchSettings() {
        super();
        this.colorize = DefaultSearchSettings.COLORIZE;
        this.firstMatch = DefaultSearchSettings.FIRSTMATCH;
        this.inLinesAfterPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inLinesBeforePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.linesAfter = DefaultSearchSettings.LINESAFTER;
        this.linesAfterToPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.linesAfterUntilPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.linesBefore = DefaultSearchSettings.LINESBEFORE;
        this.listLines = DefaultSearchSettings.LISTLINES;
        this.maxLineLength = DefaultSearchSettings.MAXLINELENGTH;
        this.multiLineSearch = DefaultSearchSettings.MULTILINESEARCH;
        this.outLinesAfterPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outLinesBeforePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.printResults = DefaultSearchSettings.PRINTRESULTS;
        this.searchArchives = DefaultSearchSettings.SEARCHARCHIVES;
        this.searchPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.textFileEncoding = DefaultSearchSettings.TEXTFILEENCODING;
        this.uniqueLines = DefaultSearchSettings.UNIQUELINES;
    }

    public boolean getColorize() {
        return colorize;
    }

    public void setColorize(boolean colorize) {
        this.colorize = colorize;
    }

    public boolean getFirstMatch() {
        return firstMatch;
    }

    public void setFirstMatch(boolean firstMatch) {
        this.firstMatch = firstMatch;
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

    public boolean getPrintResults() {
        return printResults;
    }

    public void setPrintResults(boolean printResults) {
        this.printResults = printResults;
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

    private static void addPattern(Set<Pattern> set, final String pattern) {
        set.add(Pattern.compile(pattern));
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

    public final boolean hasLinesAfterToPatterns() {
        return linesAfterToPatterns.size() > 0;
    }

    public final boolean hasLinesAfterUntilPatterns() {
        return linesAfterUntilPatterns.size() > 0;
    }

    public final boolean hasLinesAfterToOrUntilPatterns() {
        return hasLinesAfterToPatterns() || hasLinesAfterUntilPatterns();
    }

    public final void addSearchPattern(final String pattern) {
        addPattern(this.searchPatterns, pattern);
    }

    public final Set<Pattern> getSearchPatterns() {
        return this.searchPatterns;
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

    private static String localDateTimeToString(final LocalDateTime dt) {
        if (dt == null) {
            return "0";
        }
        return String.format("\"%s\"", dt);
    }

    @Override
    public final String toString() {
        return "SearchSettings("
                + "archivesOnly: " + this.getArchivesOnly()
                + ", colorize: " + this.getColorize()
                + ", debug: " + this.getDebug()
                + ", excludeHidden: " + this.getExcludeHidden()
                + ", firstMatch: " + this.getFirstMatch()
                + ", inArchiveExtensions: " + stringSetToString(this.getInArchiveExtensions())
                + ", inArchiveFilePatterns: " + patternSetToString(this.getInArchiveFilePatterns())
                + ", inDirPatterns: " + patternSetToString(this.getInDirPatterns())
                + ", inExtensions: " + stringSetToString(this.getInExtensions())
                + ", inFilePatterns: " + patternSetToString(this.getInFilePatterns())
                + ", inLinesAfterPatterns: " + patternSetToString(this.getInLinesAfterPatterns())
                + ", inLinesBeforePatterns: " + patternSetToString(this.getInLinesBeforePatterns())
                + ", linesAfter: " + this.getLinesAfter()
                + ", linesAfterToPatterns: " + patternSetToString(this.getLinesAfterToPatterns())
                + ", linesAfterUntilPatterns: " + patternSetToString(this.getLinesAfterUntilPatterns())
                + ", linesBefore: " + this.getLinesBefore()
                + ", listDirs: " + this.getListDirs()
                + ", listFiles: " + this.getListFiles()
                + ", listLines: " + this.getListLines()
                + ", maxLastMod: " + localDateTimeToString(this.getMaxLastMod())
                + ", maxLineLength: " + this.getMaxLineLength()
                + ", maxSize: " + this.getMaxSize()
                + ", minLastMod: " + localDateTimeToString(this.getMinLastMod())
                + ", minSize: " + this.getMinSize()
                + ", multiLineSearch: " + this.getMultiLineSearch()
                + ", outArchiveExtensions: " + stringSetToString(this.getOutArchiveExtensions())
                + ", outArchiveFilePatterns: " + patternSetToString(this.getOutArchiveFilePatterns())
                + ", outDirPatterns: " + patternSetToString(this.getOutDirPatterns())
                + ", outExtensions: " + stringSetToString(this.getOutExtensions())
                + ", outFilePatterns: " + patternSetToString(this.getOutFilePatterns())
                + ", outLinesAfterPatterns: " + patternSetToString(this.getOutLinesAfterPatterns())
                + ", outLinesBeforePatterns: " + patternSetToString(this.getOutLinesBeforePatterns())
                + ", paths: " + stringSetToString(this.getPaths())
                + ", printResults: " + this.getPrintResults()
                + ", printUsage: " + this.getPrintUsage()
                + ", printVersion: " + this.getPrintVersion()
                + ", recursive: " + this.getRecursive()
                + ", searchArchives: " + this.getSearchArchives()
                + ", searchPatterns: " + patternSetToString(this.getSearchPatterns())
                + ", sortBy: " + SortByUtil.toName(this.getSortBy())
                + ", sortCaseInsensitive: " + this.getSortCaseInsensitive()
                + ", sortDescending: " + this.getSortDescending()
                + ", textFileEncoding: " + getTextFileEncoding()
                + ", uniqueLines: " + this.getUniqueLines()
                + ", verbose: " + this.getVerbose()
                + ")";
    }
}
