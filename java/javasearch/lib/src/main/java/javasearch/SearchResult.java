/*******************************************************************************
SearchResult

Class to encapsulate a command line search option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.FileResult;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class SearchResult {

    final private Pattern searchPattern;
    private FileResult fileResult;
    final private int lineNum;
    final private int matchStartIndex;
    final private int matchEndIndex;
    final private String line;
    final private List<String> linesBefore;
    final private List<String> linesAfter;

    public SearchResult(final Pattern searchPattern,
                        final FileResult file,
                        final int lineNum,
                        final int matchStartIndex,
                        final int matchEndIndex,
                        final String line) {
        this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
             new ArrayList<>(), new ArrayList<>());
    }

    public SearchResult(final Pattern searchPattern,
                        final FileResult file,
                        final int lineNum,
                        final int matchStartIndex,
                        final int matchEndIndex,
                        final String line,
                        final List<String> linesBefore,
                        final List<String> linesAfter) {
        this.searchPattern = searchPattern;
        this.fileResult = file;
        this.lineNum = lineNum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
        this.line = line;
        this.linesBefore = linesBefore;
        this.linesAfter = linesAfter;
    }

    public final Pattern getSearchPattern() {
        return this.searchPattern;
    }

    public final FileResult getFileResult() {
        return this.fileResult;
    }

    protected final void setFileResult(final FileResult sf) {
        this.fileResult = sf;
    }

    public final int getLineNum() {
        return this.lineNum;
    }

    public final int getMatchStartIndex() {
        return this.matchStartIndex;
    }

    public final int getMatchEndIndex() {
        return this.matchEndIndex;
    }

    public final String getLine() {
        return this.line;
    }

    public final List<String> getLinesBefore() {
        return this.linesBefore;
    }

    public final List<String> getLinesAfter() {
        return this.linesAfter;
    }

    private int compareSearchResultFields(final SearchResult other) {
        var cmp = this.lineNum - other.lineNum;
        if (cmp == 0) {
            cmp = this.matchStartIndex - other.matchStartIndex;
            if (cmp == 0) {
                cmp = this.matchEndIndex - other.matchEndIndex;
            }
        }
        return cmp;
    }

    public int compareByPath(final SearchResult other, final boolean sortCaseInsensitive) {
        var fileResultCmp = 0;
        if (this.fileResult != null && other.fileResult != null) {
            fileResultCmp = this.fileResult.compareByPath(other.getFileResult(), sortCaseInsensitive);
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other);
        }
        return fileResultCmp;
    }

    public int compareByName(final SearchResult other, final boolean sortCaseInsensitive) {
        var fileResultCmp = 0;
        if (this.fileResult != null && other.fileResult != null) {
            fileResultCmp = this.fileResult.compareByName(other.getFileResult(), sortCaseInsensitive);
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other);
        }
        return fileResultCmp;
    }

    public int compareBySize(final SearchResult other, final boolean sortCaseInsensitive) {
        var fileResultCmp = 0;
        if (this.fileResult != null && other.fileResult != null) {
            fileResultCmp = this.fileResult.compareBySize(other.getFileResult(), sortCaseInsensitive);
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other);
        }
        return fileResultCmp;
    }

    public int compareByType(final SearchResult other, final boolean sortCaseInsensitive) {
        var fileResultCmp = 0;
        if (this.fileResult != null && other.fileResult != null) {
            fileResultCmp = this.fileResult.compareByType(other.getFileResult(), sortCaseInsensitive);
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other);
        }
        return fileResultCmp;
    }

    public int compareByLastMod(final SearchResult other, final boolean sortCaseInsensitive) {
        var fileResultCmp = 0;
        if (this.fileResult != null && other.fileResult != null) {
            fileResultCmp = this.fileResult.compareByLastMod(other.getFileResult(), sortCaseInsensitive);
        }
        if (fileResultCmp == 0) {
            return compareSearchResultFields(other);
        }
        return fileResultCmp;
    }
}
