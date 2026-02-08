/*******************************************************************************
SearchResultFormatter

Class to provide formatting of search result instances

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2020
*******************************************************************************/

package javasearch;

import javafind.Color;
import javafind.FileResultFormatter;
import javafind.StringUtil;

import java.util.function.Function;
import java.util.regex.Matcher;

public class SearchResultFormatter {
    private final SearchSettings settings;
    private final FileResultFormatter fileResultFormatter;
    private final Function<String, String> formatLineFunc;
    private final Function<String, String> formatMatchFunc;

    private static final String noSearchFileText = "<text>";

    public SearchResultFormatter(final SearchSettings settings) {
        this.settings = settings;
        this.fileResultFormatter = new FileResultFormatter(settings);
        if (settings.getColorize()) {
            this.formatLineFunc = this::formatLineWithColor;
            this.formatMatchFunc = this::formatMatchWithColor;
        } else {
            this.formatLineFunc = String::toString;
            this.formatMatchFunc = String::toString;
        }
    }

    public SearchSettings getSettings() {
        return settings;
    }

    public FileResultFormatter getFileResultFormatter() {
        return fileResultFormatter;
    }

    private String formatLineWithColor(final String line) {
        var formattedLine = line;
        for (var p : settings.getSearchPatterns()) {
            Matcher m = p.matcher(formattedLine);
            if (m.find()) {
                formattedLine = colorize(formattedLine, m.start(), m.end(), settings.getLineColor());
                break;
            }
        }
        return formattedLine;
    }

    public String formatLine(final String line) {
        return formatLineFunc.apply(line);
    }

    private String formatMatchWithColor(final String match) {
        return colorize(match, 0, match.length(), settings.getLineColor());
    }

    public String formatMatch(final String match) {
        return formatMatchFunc.apply(match);
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

    private String colorize(String s, int matchStartIndex, int matchEndIndex, Color color) {
        return FileResultFormatter.colorize(s, matchStartIndex, matchEndIndex, color);
    }

    public final String multiLineToString(SearchResult result) {
        String fileString = result.getFileResult() == null ? noSearchFileText : fileResultFormatter.formatFileResult(result.getFileResult());
        final int lineSepLength = 80;
        StringBuilder sb = new StringBuilder()
                .append("=".repeat(lineSepLength)).append("\n")
                .append(fileString).append(": ").append(result.getLineNum()).append(": ")
                .append("[").append(result.getMatchStartIndex()).append(":")
                .append(result.getMatchEndIndex()).append("]\n")
                .append("-".repeat(lineSepLength)).append("\n");
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
                    result.getMatchEndIndex() - 1, settings.getLineColor());
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

    private String formatResultMatch(SearchResult result) {
        if (StringUtil.isNullOrWhitespace(result.getLine()) || settings.getMaxLineLength() == 0) {
            return "";
        }

        int matchStartIndex = result.getMatchStartIndex() - 1;
        int matchEndIndex = result.getMatchEndIndex() - 1;
        int matchLength = matchEndIndex - matchStartIndex;

        String prefix = "";
        String suffix = "";
        int colorStartIndex = 0;
        int colorEndIndex = matchLength;

        // If matchLength longer than maxlinelength, get substring of match starting from beginning
        if (matchLength > settings.getMaxLineLength()) {
            // only do prefix and suffix if maxlinelength >= 10
            if (settings.getMaxLineLength() > 9) {
                if (matchStartIndex > 2) {
                    prefix = "...";
                }
                suffix = "...";
            }
            colorStartIndex = prefix.length();
            colorEndIndex = settings.getMaxLineLength() - suffix.length();
            matchEndIndex = matchStartIndex + colorEndIndex;
            matchStartIndex = matchStartIndex + colorStartIndex;
        }

        String match = prefix + result.getLine().substring(matchStartIndex, matchEndIndex) + suffix;
        if (settings.getColorize()) {
            match = colorize(match, colorStartIndex, colorEndIndex, settings.getLineColor());
        }
        return match;
    }

    private String formatResultLine(SearchResult result) {
        if (StringUtil.isNullOrWhitespace(result.getLine()) || settings.getMaxLineLength() == 0) {
            return "";
        }

        // if maxLineLength < 0 it means there's no limit
        boolean maxLimit = settings.getMaxLineLength() > 0;

        // If matchLength longer than maxlinelength, get substring of match starting from beginning
        if (maxLimit && (result.getMatchEndIndex() - result.getMatchStartIndex()) > settings.getMaxLineLength()) {
            return formatResultMatch(result);
        }

        // start by setting the values as if no limit
        String resultLine = result.getLine();
        int lineStartIndex = 0;
        int lineEndIndex = resultLine.length() - 1;

        while (Character.isWhitespace(resultLine.charAt(lineStartIndex))) {
            lineStartIndex++;
        }
        while (Character.isWhitespace(resultLine.charAt(lineEndIndex))) {
            lineEndIndex--;
        }

        int matchLength = result.getMatchEndIndex() - result.getMatchStartIndex();
        int matchStartIndex = result.getMatchStartIndex() - 1 - lineStartIndex;
        int matchEndIndex = matchStartIndex + matchLength;

        String prefix = "";
        String suffix = "";

        int trimmedLength = lineEndIndex - lineStartIndex;

        if (trimmedLength == 0) {
            return "";
        }

        if (maxLimit && trimmedLength > settings.getMaxLineLength()) {
            lineStartIndex = result.getMatchStartIndex() - 1;
            lineEndIndex = lineStartIndex + matchLength;
            matchStartIndex = 0;
            matchEndIndex = matchLength;

            int currentLen = lineEndIndex - lineStartIndex;
            while (currentLen < settings.getMaxLineLength()) {
                if (lineStartIndex > 0) {
                    lineStartIndex--;
                    matchStartIndex++;
                    matchEndIndex++;
                    currentLen++;
                }
                if (currentLen < settings.getMaxLineLength() && lineEndIndex < trimmedLength) {
                    lineEndIndex++;
                    currentLen++;
                }
            }

            if (settings.getMaxLineLength() > 9) {
                if (lineStartIndex > 2) {
                    prefix = "...";
                    lineStartIndex += 3;
                }
                if (lineEndIndex < (trimmedLength - 3)) {
                    suffix = "...";
                    lineEndIndex -= 3;
                }
            }
        } else {
            lineEndIndex++;
        }

        String formatted = prefix + resultLine.substring(lineStartIndex, lineEndIndex) + suffix;

        if (settings.getColorize()) {
            formatted = colorize(formatted, matchStartIndex, matchEndIndex, settings.getLineColor());
        }

        return formatted;
    }

    public final String singleLineToString(SearchResult result) {
        StringBuilder sb = new StringBuilder();
        try {
            if (result.getFileResult() != null) {
                sb.append(fileResultFormatter.formatFileResult(result.getFileResult()));
}           else {
                sb.append(noSearchFileText);
            }
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
                    .append(formatResultLine(result));
        }
        return sb.toString();
    }
}
