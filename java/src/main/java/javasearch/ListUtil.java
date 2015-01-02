package javasearch;

import java.util.ArrayList;
import java.util.List;

public class ListUtil {
    public static <T> List<T> take(List<T> list, int count) {
        if (list.size() <= count)
            return list;
        List<T>	left = new ArrayList<T>();
        int lastIndex = 0;
        while (count > 0 && lastIndex < list.size()) {
            left.add(list.get(lastIndex));
            count--;
            lastIndex++;
        }
        return left;
    }

    public static <T> List<T> init(List<T> list) {
        if (list.size() == 0)
            throw new IllegalArgumentException("init of empty list");
        else
            return take(list, list.size() - 1);
    }

    public static <T> List<T> takeRight(List<T> list, int count) {
        if (list.size() <= count)
            return list;
        List<T>	right = new ArrayList<T>();
        int lastIndex = list.size() - 1;
        while (count > 0 && lastIndex >= 0) {
            right.add(0, list.get(lastIndex));
            count--;
            lastIndex--;
        }
        return right;
    }

    public static List<Number> lessThan(Number val, List<Number> vals) {
        List<Number> lessThan = new ArrayList<Number>();
        for (Number v : vals) {
            if (v.longValue() < val.longValue())
                lessThan.add(v);
        }
        return lessThan;
    }

    public static List<Number> lessThanOrEqualTo(Number val, List<Number> vals) {
        List<Number> lessOrEq = new ArrayList<Number>();
        for (Number v : vals) {
            if (v.longValue() <= val.longValue())
                lessOrEq.add(v);
        }
        return lessOrEq;
    }

    public static Number max(List<Number> vals) {
        long maxVal = 0L;
        for (Number v : vals) {
            if (v.longValue() > maxVal)
                maxVal = v.longValue();
        }
        return maxVal;
    }

    public static List<Number> greaterThan(Number val, List<Number> vals) {
        List<Number> greaterThans = new ArrayList<Number>();
        for (Number v : vals) {
            if (v.intValue() > val.intValue())
                greaterThans.add(v);
        }
        return greaterThans;
    }

    public static Number min(List<Number> vals) {
        long minVal = 0L;
        if (!vals.isEmpty()) {
            minVal = vals.get(0).longValue();
            for (Number v : vals) {
                if (v.longValue() < minVal)
                    minVal = v.longValue();
            }
        }
        return minVal;
    }
}
