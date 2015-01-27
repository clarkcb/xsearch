package javasearch;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertEquals;

public class StringUtilTest {
    public StringUtilTest() {}

    @Test
    public void testTrimNewLineWithNewLine() {
        String s = "This is a line with a newline\n";
        String trimmed = StringUtil.trimNewLine(s);
        assertEquals(trimmed, "This is a line with a newline");
    }
}