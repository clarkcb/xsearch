/*******************************************************************************
SearchResult

Class to encapsulate a command line search option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class SearchResult {

    final private Pattern searchPattern;
    private SearchFile searchFile;
    final private int lineNum;
    final private int matchStartIndex;
    final private int matchEndIndex;
    final private String line;
    final private List<String> linesBefore;
    final private List<String> linesAfter;

    public SearchResult(final Pattern searchPattern,
                        final SearchFile file,
                        final int lineNum,
                        final int matchStartIndex,
                        final int matchEndIndex,
                        final String line) {
        this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
             new ArrayList<>(), new ArrayList<>());
    }

    public SearchResult(final Pattern searchPattern,
                        final SearchFile file,
                        final int lineNum,
                        final int matchStartIndex,
                        final int matchEndIndex,
                        final String line,
                        final List<String> linesBefore,
                        final List<String> linesAfter) {
        this.searchPattern = searchPattern;
        this.searchFile = file;
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

    public final SearchFile getSearchFile() {
        return this.searchFile;
    }

    protected final void setSearchFile(final SearchFile sf) {
        this.searchFile = sf;
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
}
