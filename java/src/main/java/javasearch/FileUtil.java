/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.*;
import org.xml.sax.SAXException;

public class FileUtil {

    private static Set<String> dotDirs = new HashSet<String>(Arrays.asList(".", ".."));

    public static String getExtension(File f) {
        String ext = "";
        String fileName = f.getName();
        int lastIndex = fileName.lastIndexOf(".");
        if (lastIndex > 0 && lastIndex < fileName.length()-1)
            ext = fileName.substring(lastIndex + 1);
        return ext;
    }

    public static boolean isDotDir(File f) {
        return isDotDir(f.getName());
    }

    public static boolean isDotDir(String f) {
        return dotDirs.contains(f);
    }

    public static boolean isHidden(File f) {
        return isHidden(f.getName());
    }

    public static boolean isHidden(String f) {
        return f.length() > 1 && f.charAt(0) == '.' && !isDotDir(f);
    }
}
