package javasearch;

public class SearchException extends Exception {
    String msg;
    public SearchException(String msg) {
        this.msg = msg;
    }
    public String getMessage() {
        return this.msg;
    }
}
