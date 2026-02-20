using System.Collections.Generic;
using System.IO;
using CsFindLib;
using CsSearchLib;
using NUnit.Framework;

namespace CsSearchTests;

[TestFixture]
public class SearchResultFormatterTests
{
    private const string CsSearchLibPath = "~/src/xsearch/csharp/CsSearch/CsSearchLib";

    [Test]
    public void SearchResultSingleLine_Format_EqualsExpected()
    {
        const string pattern = "Search";
        var filePath = Path.Join(CsSearchLibPath, "Searcher.cs");
        const int lineNum = 14;
        const int matchStartIndex = 14;
        const int matchEndIndex = 20;
        const string line = "public class Searcher\n";
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Code, lineNum,
            matchStartIndex, matchEndIndex, line);
        var expandedSearchPath = FileUtil.ExpandPath(CsSearchLibPath);
        var expectedPath = Path.Join(expandedSearchPath, "Searcher.cs");
        var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {line.Trim()}";

        var settings = new SearchSettings
        {
            Colorize = false
        };
        var formatter = new SearchResultFormatter(settings);

        Assert.That(formatter.Format(searchResult), Is.EqualTo(expectedOutput));
    }

    [Test]
    public void SearchResultSingleLineLongerThanMaxLineLength_Format_EqualsExpected()
    {
        const string pattern = "maxlen";
        const string filePath = "./maxlen.txt";
        const int lineNum = 1;
        const int matchStartIndex = 53;
        const int matchEndIndex = 59;
        const string line =
            "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Text, lineNum,
            matchStartIndex, matchEndIndex, line);
        const string expectedPath = "./maxlen.txt";
        const string expectedLine =
            "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
        var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {expectedLine}";

        var settings = new SearchSettings
        {
            MaxLineLength = 100,
            Colorize = false
        };
        var formatter = new SearchResultFormatter(settings);

        Assert.That(formatter.Format(searchResult), Is.EqualTo(expectedOutput));
    }

    [Test]
    public void SearchResultSingleLineLongerColorize_Format_EqualsExpected()
    {
        const string pattern = "maxlen";
        const string filePath = "./maxlen.txt";
        const int lineNum = 1;
        const int matchStartIndex = 53;
        const int matchEndIndex = 59;
        const string line =
            "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Text, lineNum,
            matchStartIndex, matchEndIndex, line);
        const string expectedPath = "./maxlen.txt";
        const string expectedLine = "...89012345678901234567890123456789012345678901" +
                                    ConsoleColor.Green +
                                    "maxlen" +
                                    ConsoleColor.Reset +
                                    "89012345678901234567890123456789012345678901...";
        var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {expectedLine}";

        var settings = new SearchSettings
        {
            MaxLineLength = 100,
            Colorize = true
        };
        var formatter = new SearchResultFormatter(settings);

        var output = formatter.Format(searchResult);
        Assert.That(output, Is.EqualTo(expectedOutput));
    }

    [Test]
    public void SearchResultMatchLongerColorize_Format_EqualsExpected()
    {
        const string pattern = @"\d+maxlen\d+";
        const string filePath = "./maxlen.txt";
        const int lineNum = 1;
        const int matchStartIndex = 1;
        const int matchEndIndex = 110;
        const string line =
            "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Text, lineNum,
            matchStartIndex, matchEndIndex, line);
        const string expectedPath = "./maxlen.txt";
        const string expectedLine = ConsoleColor.Green +
                                    "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
                                    ConsoleColor.Reset +
                                    "...";
        var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {expectedLine}";

        var settings = new SearchSettings
        {
            MaxLineLength = 100,
            Colorize = true
        };
        var formatter = new SearchResultFormatter(settings);

        var output = formatter.Format(searchResult);
        Assert.That(output, Is.EqualTo(expectedOutput));
    }

    [Test]
    public void SearchResultMatchLongerColorize2_Format_EqualsExpected()
    {
        const string pattern = @"\d+maxlen\d+";
        const string filePath = "./maxlen.txt";
        const int lineNum = 1;
        const int matchStartIndex = 11;
        const int matchEndIndex = 120;
        const string line =
            "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ";
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Text, lineNum,
            matchStartIndex, matchEndIndex, line);
        const string expectedPath = "./maxlen.txt";
        const string expectedLine = "..." +
                                    ConsoleColor.Green +
                                    "3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456" +
                                    ConsoleColor.Reset +
                                    "...";
        var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {expectedLine}";

        var settings = new SearchSettings
        {
            MaxLineLength = 100,
            Colorize = true
        };
        var formatter = new SearchResultFormatter(settings);

        var output = formatter.Format(searchResult);
        Assert.That(output, Is.EqualTo(expectedOutput));
    }

    [Test]
    public void SearchResultBinaryFile_Format_EqualsExpected()
    {
        const string pattern = "Search";
        var filePath = Path.Join(CsSearchLibPath, "Searcher.exe");
        const int lineNum = 0;
        const int matchStartIndex = 0;
        const int matchEndIndex = 0;
        string? line = null;
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Binary,
            lineNum, matchStartIndex, matchEndIndex, line);
        var expandedSearchPath = FileUtil.ExpandPath(CsSearchLibPath);
        var expectedPath = Path.Join(expandedSearchPath, "Searcher.exe");
        var expectedOutput = $"{expectedPath} matches at [0:0]";

        var settings = new SearchSettings();
        var formatter = new SearchResultFormatter(settings);

        var output = formatter.Format(searchResult);
        Assert.That(output, Is.EqualTo(expectedOutput));
    }

    [Test]
    public void SearchResultMultiLine_Format_EqualsExpected()
    {
        const string pattern = "Search";
        var filePath = Path.Join(CsSearchLibPath, "Searcher.cs");
        const int lineNum = 14;
        const int matchStartIndex = 14;
        const int matchEndIndex = 20;
        const string line = "\tpublic class Searcher";
        var linesBefore = new List<string> { "namespace CsSearch", "{" };
        var linesAfter = new List<string> {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};
        var searchResult = SearchResultTests.GetSearchResult(pattern, filePath, FileType.Code, lineNum,
            matchStartIndex, matchEndIndex, line, linesBefore, linesAfter);

        var expandedSearchPath = FileUtil.ExpandPath(CsSearchLibPath);
        var expectedPath = Path.Join(expandedSearchPath, "Searcher.cs");
        var expectedOutput = string.Format(new string('=', 80) + "\n" +
                                           "{0}: {1}: [{2}:{3}]\n" +
                                           new string('-', 80) + "\n" +
                                           "  12 | namespace CsSearch\n" +
                                           "  13 | {{\n" +
                                           "> 14 | 	public class Searcher\n" +
                                           "  15 | 	{{\n" +
                                           "  16 | 		private readonly FileTypes _fileTypes;\n",
            expectedPath, lineNum, matchStartIndex, matchEndIndex);

        var settings = new SearchSettings
        {
            Colorize = false
        };
        var formatter = new SearchResultFormatter(settings);
		
        var output = formatter.Format(searchResult);
        Assert.That(output, Is.EqualTo(expectedOutput));
    }
}