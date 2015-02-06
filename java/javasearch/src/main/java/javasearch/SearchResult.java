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

    // temp
    private final int MAXLINELENGTH = 150;

	public SearchResult(Pattern searchPattern, SearchFile file, int lineNum,
                        int matchStartIndex, int matchEndIndex, String line) {
		this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
                new ArrayList<String>(), new ArrayList<String>());
	}

	public SearchResult(Pattern searchPattern, SearchFile file, int lineNum,
                        int matchStartIndex, int matchEndIndex, String line,
                        List<String> linesBefore, List<String> linesAfter) {
		this.searchPattern = searchPattern;
		this.searchFile = file;
        this.lineNum = lineNum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
		this.line = line;
		this.linesBefore = linesBefore;
		this.linesAfter = linesAfter;
	}

	public Pattern getSearchPattern() {
		return this.searchPattern;
	}

	public SearchFile getSearchFile() {
		return this.searchFile;
	}

	protected void setSearchFile(SearchFile sf) {
		this.searchFile = sf;
	}

	public int getLineNum() {
		return this.lineNum;
	}

	public int getMatchStartIndex() {
		return this.matchStartIndex;
	}

	public int getMatchEndIndex() {
		return this.matchEndIndex;
	}

	public String getLine() {
		return this.line;
	}

	public List<String> getLinesBefore() {
		return this.linesBefore;
	}

	public List<String> getLinesAfter() {
		return this.linesAfter;
	}

	public String toString() {
		if (linesBefore.size() > 0 || linesAfter.size() > 0)
			return multiLineToString();
		else
			return singleLineToString();
	}

	private String repeatString(String s, int times) {
		StringBuilder sb = new StringBuilder();
		for (int i=0; i < times; i++) {
			sb.append(s);
		}
		return sb.toString();
	}

	private int lineNumPadding() {
		int maxLineNum = lineNum + linesAfter.size();
		return String.format("%d", maxLineNum).length();
	}

	public String multiLineToString() {
		StringBuilder sb = new StringBuilder()
				.append(repeatString("=", 80)).append("\n")
				.append(searchFile).append(": ").append(lineNum).append(": ")
				.append("[").append(matchStartIndex).append(":")
				.append(matchEndIndex).append("]\n")
				.append(repeatString("-", 80)).append("\n");
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

	public String singleLineToString() {
		StringBuilder sb = new StringBuilder();
		try {
			//sb.append(this.file.getCanonicalPath());
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
        if (lineLength > this.MAXLINELENGTH) {
            int adjustedMaxLength = this.MAXLINELENGTH - matchLength;
            int beforeIndex = this.matchStartIndex;
            if (this.matchStartIndex > 0) {
                beforeIndex = beforeIndex - (adjustedMaxLength / 4);
                if (beforeIndex < 0)
                    beforeIndex = 0;
            }
            adjustedMaxLength = adjustedMaxLength - (this.matchStartIndex - beforeIndex);
            int afterIndex = this.matchEndIndex + adjustedMaxLength;
            if (afterIndex > lineLength)
                afterIndex = lineLength;

            String before = "";
            if (beforeIndex > 3) {
                before = "...";
                beforeIndex += 3;
            }
            String after = "";
            if (afterIndex < lineLength - 3) {
                after = "...";
                afterIndex -= 3;
            }
            formatted = before + this.line.substring(beforeIndex, afterIndex) + after;
        }
        return formatted.trim();
    }
}
