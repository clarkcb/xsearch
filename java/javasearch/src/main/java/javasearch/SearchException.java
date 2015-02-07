package javasearch;

public class SearchException extends Exception {
    private String msg;
    public SearchException(final String msg) {
        this.msg = msg;
    }
    public final String getMessage() {
        return this.msg;
    }
}
