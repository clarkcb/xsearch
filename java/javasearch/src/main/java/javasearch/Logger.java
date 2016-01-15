package javasearch;

public class Logger {
    public static void log(final String message) {
        System.out.println(message);
    }

    public static void logError(final String message) {
        log("ERROR: " + message);
    }
}
