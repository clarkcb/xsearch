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

    private Pattern searchPattern;
    private SearchFile searchFile;
    private int lineNum;
    private int matchStartIndex;
    private int matchEndIndex;
    private String line;
    private List<String> linesBefore;
    private List<String> linesAfter;

    public SearchResult(final Pattern searchPattern,
                        final SearchFile file,
                        final int lineNum,
                        final int matchStartIndex,
                        final int matchEndIndex,
                        final String line) {
        this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
             new ArrayList<String>(), new ArrayList<String>());
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

    public final String toString() {
        if (linesBefore.size() > 0 || linesAfter.size() > 0) {
            return multiLineToString();
        } else {
            return singleLineToString();
        }
    }

    private String repeatString(final String s, final int times) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < times; i++) {
            sb.append(s);
        }
        return sb.toString();
    }

    private int lineNumPadding() {
        int maxLineNum = lineNum + linesAfter.size();
        return String.format("%d", maxLineNum).length();
    }

    public final String multiLineToString() {
        int lineSepLength = 80;
        StringBuilder sb = new StringBuilder()
                .append(repeatString("=", lineSepLength)).append("\n")
                .append(searchFile).append(": ").append(lineNum).append(": ")
                .append("[").append(matchStartIndex).append(":")
                .append(matchEndIndex).append("]\n")
                .append(repeatString("-", lineSepLength)).append("\n");
        int currentLineNum = lineNum;
        String lineFormat = " %1$" + lineNumPadding() + "d | %2$s\n";
        if (!linesBefore.isEmpty()) {
            currentLineNum -= linesBefore.size();
            for (String lineBefore : linesBefore) {
                sb.append(" ")
                        .append(String.format(lineFormat, currentLineNum,
                                StringUtil.trimNewLine(lineBefore)));
                currentLineNum++;
            }
        }
        sb.append(">").append(String.format(lineFormat, lineNum,
                StringUtil.trimNewLine(line)));
        if (!linesAfter.isEmpty()) {
            currentLineNum++;
            for (String lineAfter : linesAfter) {
                sb.append(" ")
                        .append(String.format(lineFormat, currentLineNum,
                                StringUtil.trimNewLine(lineAfter)));
                currentLineNum++;
            }
        }
        return sb.toString();
    }

    public final String singleLineToString() {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append(this.searchFile.toString());
        } catch (NullPointerException e) {
            sb.append("");
        }
        if (this.lineNum == 0) {
            sb.append(" matches");
        } else {
            sb.append(": ");
            sb.append(this.lineNum);
            sb.append(": [");
            sb.append(this.matchStartIndex);
            sb.append(":");
            sb.append(this.matchEndIndex);
            sb.append("]: ");
            sb.append(formatMatchingLine());
        }
        return sb.toString();
    }

    private String formatMatchingLine() {
        String formatted = this.line;
        int lineLength = this.line.length();
        int matchLength = this.matchEndIndex - this.matchStartIndex;
        int maxLineLength = 150;
        if (lineLength > maxLineLength) {
            int adjustedMaxLength = maxLineLength - matchLength;
            int beforeIndex = this.matchStartIndex;
            if (this.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0) {
                    beforeIndex = 0;
                }
            }
            adjustedMaxLength = adjustedMaxLength - (this.matchStartIndex - beforeIndex);
            int afterIndex = this.matchEndIndex + adjustedMaxLength;
            if (afterIndex > lineLength) {
                afterIndex = lineLength;
            }

            final String ellipses = "...";
            String before = "";
            if (beforeIndex > ellipses.length()) {
                before = ellipses;
                beforeIndex += ellipses.length();
            }
            String after = "";
            if (afterIndex < lineLength - ellipses.length()) {
                after = ellipses;
                afterIndex -= ellipses.length();
            }
            formatted = before + this.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted.trim();
    }
}
