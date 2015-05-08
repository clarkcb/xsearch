/*******************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.Scanner;

public class FileUtil {

    private static Set<String> dotDirs = new HashSet<String>(Arrays.asList(".", ".."));
    private static final String DEFAULT_ENCODING = "UTF-8";

    public static String getExtension(final File f) {
        String ext = "";
        String fileName = f.getName();
        int lastIndex = fileName.lastIndexOf(".");
        if (lastIndex > 0 && lastIndex < fileName.length() - 1) {
            ext = fileName.substring(lastIndex + 1);
        }
        return ext;
    }

    public static boolean isDotDir(final String f) {
        return dotDirs.contains(f);
    }

    public static boolean isHidden(final File f) {
        return isHidden(f.getName());
    }

    public static boolean isHidden(final String f) {
        return f.length() > 1 && f.charAt(0) == '.' && !isDotDir(f);
    }

    public static List<String> splitPath(final String path) {
        String[] elems = path.split(File.separator);
        List<String> nonDotDirElems = new ArrayList<String>();
        for (String elem : elems) {
            if (!isDotDir(elem) && !elem.equals("")) { nonDotDirElems.add(elem); }
        }
        return nonDotDirElems;
    }

    public static String getFileContents(final File f) throws IOException {
        return getFileContents(f, DEFAULT_ENCODING);
    }

    public static String getFileContents(final File f, final String enc) throws IOException {
        Scanner scanner = new Scanner(f, enc).useDelimiter("\\Z");
        String content;
        try {
            content = scanner.next();
        } catch (NoSuchElementException e) {
            throw e;
        } catch (IllegalStateException e) {
            throw e;
        } finally {
            scanner.close();
        }
        return content;
    }

    public static String getStreamContents(final InputStream is) throws IllegalArgumentException {
        return getStreamContents(is, DEFAULT_ENCODING);
    }

    public static String getStreamContents(final InputStream is, final String enc) throws IllegalArgumentException {
        Scanner scanner = new Scanner(is, enc).useDelimiter("\\Z");
        String content;
        try {
            content = scanner.next();
        } catch (NoSuchElementException e) {
            throw e;
        } catch (IllegalStateException e) {
            throw e;
        } finally {
            scanner.close();
        }
        return content;
    }

    // NOTE: user takes responsibility for closing the LineIterator once done
    public static LineIterator getFileLineIterator(final File f) throws IOException {
        return getFileLineIterator(f, DEFAULT_ENCODING);
    }

    // NOTE: user takes responsibility for closing the LineIterator once done
    public static LineIterator getFileLineIterator(final File f, final String enc) throws IOException {
        return FileUtils.lineIterator(f, enc);
    }

    public static List<String> getStreamLines(final InputStream is) throws IllegalArgumentException {
        return getStreamLines(is, DEFAULT_ENCODING);
    }

    public static List<String> getStreamLines(final InputStream is, final String enc) throws IllegalArgumentException {
        List<String> lines = new ArrayList<String>();
        Scanner scanner = new Scanner(is, enc).useDelimiter("\r?\n");
        while (scanner.hasNext()) {
            try {
                lines.add(scanner.next());
            } catch (NoSuchElementException e) {
                break;
            }
        }
        scanner.close();
        return lines;
    }
}
