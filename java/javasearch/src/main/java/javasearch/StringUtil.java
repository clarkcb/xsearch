/* *****************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
***************************************************************************** */

package javasearch;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

final class StringUtil {
    private StringUtil() {
        // inaccessible constructor for utility class
    }

    private static final Set<Character> NEWLINECHARS = new HashSet<>(Arrays.asList('\n', '\r'));
    static String trimNewLine(final String s) {
        String trimmed = s;
        while (trimmed.length() > 0 && NEWLINECHARS.contains(trimmed.charAt(trimmed.length() - 1))) {
            trimmed = trimmed.substring(0, trimmed.length() - 1);
        }
        return trimmed;
    }
}
