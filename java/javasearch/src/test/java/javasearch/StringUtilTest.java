package javasearch;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class StringUtilTest {
    public StringUtilTest() {}

    @Test
    public final void testTrimNewLineWithNewLine() {
        final String s = "This is a line with a newline\n";
        final String trimmed = StringUtil.trimNewLine(s);
        assertEquals("This is a line with a newline", trimmed);
    }
}
