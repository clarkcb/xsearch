/*******************************************************************************
SearchResultFormatter

Class to provide formatting of search result instances

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2020
*******************************************************************************/

package javasearch;

import javafind.StringUtil;

public class SearchResultFormatter {

    private final SearchSettings settings;

    private static final String noSearchFileText = "<text>";

    public SearchResultFormatter(final SearchSettings settings) {
        this.settings = settings;
    }

    public final String format(SearchResult result) {
        if (!result.getLinesBefore().isEmpty() || !result.getLinesAfter().isEmpty()) {
            return multiLineToString(result);
        } else {
            return singleLineToString(result);
        }
    }

    private int lineNumPadding(SearchResult result) {
        int maxLineNum = result.getLineNum() + result.getLinesAfter().size();
        return String.format("%d", maxLineNum).length();
    }

    private String colorize(String s, int matchStartIndex, int matchEndIndex) {
        return s.substring(0, matchStartIndex) +
                Color.GREEN +
                s.substring(matchStartIndex, matchEndIndex) +
                Color.RESET +
                s.substring(matchEndIndex);
    }

    public final String multiLineToString(SearchResult result) {
        String fileString = result.getFileResult() == null ? noSearchFileText : result.getFileResult().toString();
        final int lineSepLength = 80;
        StringBuilder sb = new StringBuilder()
                .append(StringUtil.repeatString("=", lineSepLength)).append("\n")
                .append(fileString).append(": ").append(result.getLineNum()).append(": ")
                .append("[").append(result.getMatchStartIndex()).append(":")
                .append(result.getMatchEndIndex()).append("]\n")
                .append(StringUtil.repeatString("-", lineSepLength)).append("\n");
        int currentLineNum = result.getLineNum();
        String lineFormat = " %1$" + lineNumPadding(result) + "d | %2$s\n";
        if (!result.getLinesBefore().isEmpty()) {
            currentLineNum -= result.getLinesBefore().size();
            for (String lineBefore : result.getLinesBefore()) {
                sb.append(" ")
                        .append(String.format(lineFormat, currentLineNum,
                                StringUtil.trimNewLine(lineBefore)));
                currentLineNum++;
            }
        }
        String line = StringUtil.trimNewLine(result.getLine());
        if (settings.getColorize()) {
            line = colorize(line, result.getMatchStartIndex() - 1,
                    result.getMatchEndIndex() - 1);
        }

        sb.append(">").append(String.format(lineFormat, result.getLineNum(), line));
        if (!result.getLinesAfter().isEmpty()) {
            currentLineNum++;
            for (String lineAfter : result.getLinesAfter()) {
                sb.append(" ")
                        .append(String.format(lineFormat, currentLineNum,
                                StringUtil.trimNewLine(lineAfter)));
                currentLineNum++;
            }
        }
        return sb.toString();
    }

    private String formatMatchingLine(SearchResult result) {
        String formatted = result.getLine();
        int leadingWhitespaceCount = 0;
        while (Character.isWhitespace(formatted.charAt(leadingWhitespaceCount))) {
            leadingWhitespaceCount++;
        }
        formatted = formatted.trim();
        int formattedLength = formatted.length();
        int maxLineEndIndex = formattedLength - 1;
        int matchLength = result.getMatchEndIndex() - result.getMatchStartIndex();
        int matchStartIndex = result.getMatchStartIndex() - 1 - leadingWhitespaceCount;
        int matchEndIndex = matchStartIndex + matchLength;

        // If longer than maxlinelength, walk out from match indices
        if (formattedLength > settings.getMaxLineLength()) {
            int lineStartIndex = matchStartIndex;
            int lineEndIndex = lineStartIndex + matchLength;
            matchStartIndex = 0;
            matchEndIndex = matchLength;

            while (lineEndIndex > formattedLength - 1) {
                lineStartIndex--;
                lineEndIndex--;
                matchStartIndex++;
                matchEndIndex++;
            }

            formattedLength = lineEndIndex - lineStartIndex;
            while (formattedLength < settings.getMaxLineLength()) {
                if (lineStartIndex > 0) {
                    lineStartIndex--;
                    matchStartIndex++;
                    matchEndIndex++;
                    formattedLength = lineEndIndex - lineStartIndex;
                }
                if (formattedLength < settings.getMaxLineLength() && lineEndIndex < maxLineEndIndex) {
                    lineEndIndex++;
                }
                formattedLength = lineEndIndex - lineStartIndex;
            }

            formatted = formatted.substring(lineStartIndex, lineEndIndex);

            if (lineStartIndex > 2) {
                formatted = "..." + formatted.substring(3);
            }
            if (lineEndIndex < maxLineEndIndex - 3) {
                formatted = formatted.substring(0, formattedLength - 3) + "...";
            }
        }

        if (settings.getColorize()) {
            formatted = colorize(formatted, matchStartIndex, matchEndIndex);
        }

        return formatted;
    }

    public final String singleLineToString(SearchResult result) {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append(result.getFileResult().toString());
        } catch (NullPointerException e) {
            sb.append(noSearchFileText);
        }
        if (result.getLineNum() == 0) {
            sb.append(" matches at [")
                    .append(result.getMatchStartIndex())
                    .append(":")
                    .append(result.getMatchEndIndex())
                    .append("]");
        } else {
            sb.append(": ")
                    .append(result.getLineNum())
                    .append(": [")
                    .append(result.getMatchStartIndex())
                    .append(":")
                    .append(result.getMatchEndIndex())
                    .append("]: ")
                    .append(formatMatchingLine(result));
        }
        return sb.toString();
    }
}
