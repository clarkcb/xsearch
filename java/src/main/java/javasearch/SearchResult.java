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
	private String line;

	public SearchResult(Pattern searchPattern, File file, int lineNum,
		String line) {
		this.searchPattern = searchPattern;
		this.file = file;
		this.lineNum = lineNum;
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
			sb.append(this.file.getCanonicalPath());
		} catch (Exception e) {
			sb.append(this.file.getAbsolutePath());
		}
		if (this.lineNum == 0) {
			sb.append(" matches");
		} else {
			sb.append(": ");
			sb.append(this.lineNum);
			sb.append(": ");
			sb.append(this.line.trim());
		}
		return sb.toString();
	}
}
