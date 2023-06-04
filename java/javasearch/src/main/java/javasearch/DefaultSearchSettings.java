package javasearch;

import javafind.DefaultFindSettings;

public final class DefaultSearchSettings {
    private DefaultSearchSettings() {
        // inaccessible constructor for utility class
    }

    public static final boolean COLORIZE = true;
    public static final boolean FIRSTMATCH = false;
    public static final int LINESAFTER = 0;
    public static final int LINESBEFORE = 0;
    public static final boolean LISTLINES = false;
    public static final int MAXLINELENGTH = 150;
    public static final boolean MULTILINESEARCH = false;
    public static final boolean PRINTRESULTS = true;
    public static final boolean SEARCHARCHIVES = false;
    public static final String TEXTFILEENCODING = "UTF-8";
    public static final boolean UNIQUELINES = false;
}
