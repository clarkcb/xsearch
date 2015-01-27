/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class StringUtil {
	public static Set<Character> NEWLINECHARS = new HashSet<Character>(Arrays.asList('\n', '\r'));
	public static String trimNewLine(String s) {
		String trimmed = s;
		while (trimmed.length() > 0 && NEWLINECHARS.contains(trimmed.charAt(trimmed.length() - 1))) {
			trimmed = trimmed.substring(0, trimmed.length()-1);
		}
		return trimmed;
	}
}
