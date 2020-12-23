package javasearch;

import java.util.ArrayList;
import java.util.List;

public final class ListUtil {

    private ListUtil() {
        // inaccessible constructor for utility class
    }

    // return a new list with count number of elements dropped from the beginning,
    // or an empty list if the list is shorter than count
    public static <T> List<T> drop(final List<T> list, final int count) {
        if (count < 1) return list;
        if (count >= list.size()) return new ArrayList<>();
        return list.subList(count, list.size());
    }

    // return a new list with only the first count number of elements,
    // or the list if it is shorter than count
    public static <T> List<T> take(final List<T> list, final int count) {
        if (count >= list.size()) return list;
        if (count < 1) return new ArrayList<>();
        return list.subList(0, count);
    }

    // return a new list with all but the last element in it
    public static <T> List<T> init(final List<T> list) {
        if (list.size() == 0) {
            throw new IllegalArgumentException("init of empty list");
        } else {
            return take(list, list.size() - 1);
        }
    }

    // return a new list with all but the first element in it
    public static <T> List<T> tail(final List<T> list) {
        return drop(list, 1);
    }

    public static <T> List<T> takeRight(final List<T> list, final int count) {
        if (list.size() <= count) {
            return list;
        }
        return list.subList(list.size() - count, list.size());
    }
}
