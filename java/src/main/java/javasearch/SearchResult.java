/*******************************************************************************
SearchResult

Class to encapsulate a command line search option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.File;
import java.util.regex.Pattern;

public class SearchResult {

	private Pattern searchPattern;
	private File file;
    private int lineNum;
    private int matchStartIndex;
    private int matchEndIndex;
	private String line;

    // temp
    private int MAXLINELENGTH = 150;

	public SearchResult(Pattern searchPattern, File file, int lineNum,
                        int matchStartIndex, int matchEndIndex, String line) {
		this.searchPattern = searchPattern;
		this.file = file;
        this.lineNum = lineNum;
        this.matchStartIndex = matchStartIndex;
        this.matchEndIndex = matchEndIndex;
		this.line = line;
	}

	public Pattern getSearchPattern() {
		return this.searchPattern;
	}

	public File getFile() {
		return this.file;
	}

	public int getLineNum() {
		return this.lineNum;
	}

	public String getLine() {
		return this.line;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		try {
			//sb.append(this.file.getCanonicalPath());
			sb.append(this.file.getPath());
		} catch (Exception e) {
			sb.append(this.file.getAbsolutePath());
		}
		if (this.lineNum == 0) {
			sb.append(" matches");
		} else {
			sb.append(": ");
			sb.append(this.lineNum);
			sb.append(" [");
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
