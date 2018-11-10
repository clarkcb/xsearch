package javasearch;

public class SearchException extends Exception {

    public SearchException() {
        super();
    }

    public SearchException(final String msg) {
        super(msg);
    }

    public SearchException(final String msg, final Exception cause) {
        super(msg, cause);
    }
}
