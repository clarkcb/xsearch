package javasearch;

import javafind.Color;

public final class DefaultSearchSettings {
    private DefaultSearchSettings() {
        // inaccessible constructor for utility class
    }

    public static final boolean FIRST_MATCH = false;
    public static final Color LINE_COLOR = Color.GREEN;
    public static final int LINES_AFTER = 0;
    public static final int LINES_BEFORE = 0;
    public static final boolean PRINT_LINES = false;
    public static final boolean PRINT_MATCHES = false;
    public static final int MAX_LINE_LENGTH = 150;
    public static final boolean MULTI_LINE_SEARCH = false;
    public static final boolean PRINT_RESULTS = true;
    public static final boolean SEARCH_ARCHIVES = false;
    public static final String TEXT_FILE_ENCODING = "UTF-8";
    public static final boolean UNIQUE_LINES = false;
}
