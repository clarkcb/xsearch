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

public final class FileUtil {

    private FileUtil() {
        // inaccessible constructor for utility class
    }

    private static final Set<String> dotDirs = new HashSet<>(Arrays.asList(".", ".."));
    private static final String DEFAULT_ENCODING = "UTF-8";

    public static String getExtension(final File f) {
        return getExtension(f.getName());
    }

    public static String getExtension(final String fileName) {
        String ext = "";
        int lastIndex = fileName.lastIndexOf(".");
        if (lastIndex > 0 && lastIndex < fileName.length() - 1) {
            ext = fileName.substring(lastIndex + 1);
            if (!ext.equals("Z")) { // the only always-uppercase ext
                ext = ext.toLowerCase();
            }
        }
        return ext;
    }

    public static boolean hasExtension(final File f, String ext) {
        return hasExtension(f.getName(), ext);
    }

    public static boolean hasExtension(final String fileName, String ext) {
        if (!ext.equals("Z")) { // the only always-uppercase ext
            ext = ext.toLowerCase();
        }
        return getExtension(fileName).equals(ext);
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

    // NOTE: if the first item in the returned list is not a dotDir, it should be
    // considered an absolute path
    public static List<String> splitPath(final String path) {
        if (path == null || path.isEmpty()) {
            return new ArrayList<>();
        }
        String[] elems = path.split(File.separator);
        List<String> elemList = new ArrayList<>();
        for (String elem : elems) {
            if (!elem.isEmpty()) { elemList.add(elem); }
        }
        return elemList;
    }

    public static String getFileContents(final File f) throws IOException {
        return getFileContents(f, DEFAULT_ENCODING);
    }

    public static String getFileContents(final File f, final String enc) throws IOException {
        String content;
        try (Scanner scanner = new Scanner(f, enc).useDelimiter("\\Z")) {
            content = getScannerContents(scanner);
        } catch (NoSuchElementException | IllegalStateException e) {
            throw e;
        }
        return content;
    }

    public static String getScannerContents(final Scanner scanner) throws IllegalArgumentException {
        String content;
        if (scanner.hasNext()) { // will be false if file size == 0
            content = scanner.next();
        } else {
            content = "";
        }
        return content;
    }

    public static String getStreamContents(final InputStream is) throws IllegalArgumentException {
        return getStreamContents(is, DEFAULT_ENCODING);
    }

    public static String getStreamContents(final InputStream is, final String enc) throws IllegalArgumentException {
        String content;
        try (Scanner scanner = new Scanner(is, enc).useDelimiter("\\Z")) {
            content = getScannerContents(scanner);
        } catch (NoSuchElementException | IllegalStateException e) {
            throw e;
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
        List<String> lines = new ArrayList<>();
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
