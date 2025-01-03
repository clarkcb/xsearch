using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using CsSearchLib;
using NUnit.Framework;

using CsFindLib;

namespace CsSearchTests;

[TestFixture]
class SearchResultTests
{
	private const string CsSearchPath = "~/src/xsearch/csharp/CsSearch/CsSearch";

	[Test]
	public void SearchResultSingleLine_ToString_EqualsExpected()
	{
		var settings = new SearchSettings
		{
			MaxLineLength = 100,
			Colorize = false
		};
		var formatter = new SearchResultFormatter(settings);
		var pattern = new Regex("Search");
		var filePath = new FilePath(Path.Join(CsSearchPath, "Searcher.cs"));
		var file = new FileResult(filePath, FileType.Code);
		const int lineNum = 10;
		const int matchStartIndex = 15;
		const int matchEndIndex = 23;
		const string line = "\tpublic class Searcher\n";
		var searchResult = new SearchResult(pattern, file, lineNum,
			matchStartIndex, matchEndIndex, line);
		var expandedSearchPath = FileUtil.ExpandPath(CsSearchPath);
		var expectedPath = Path.Join(expandedSearchPath, "Searcher.cs");
		var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {line.Trim()}";

		Assert.That(formatter.Format(searchResult), Is.EqualTo(expectedOutput));
	}

	[Test]
	public void SearchResultSingleLineLongerThanMaxLineLength_ToString_EqualsExpected()
	{
		var settings = new SearchSettings
		{
			MaxLineLength = 100,
			Colorize = false
		};
		var formatter = new SearchResultFormatter(settings);
		var pattern = new Regex("maxlen");
		var filePath = new FilePath("./maxlen.txt");
		var file = new FileResult(filePath, FileType.Text);
		const int lineNum = 1;
		const int matchStartIndex = 53;
		const int matchEndIndex = 59;
		const string line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
		var linesBeforeAfter = new List<string>();

		var searchResult = new SearchResult(pattern, file, lineNum, matchStartIndex,
			matchEndIndex, line, linesBeforeAfter, linesBeforeAfter);
		const string expectedPath = "./maxlen.txt";
		const string expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...";
		var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {expectedLine}";

		Assert.That(formatter.Format(searchResult), Is.EqualTo(expectedOutput));
	}

	[Test]
	public void SearchResultSingleLineLongerColorize_ToString_EqualsExpected()
	{
		var settings = new SearchSettings
		{
			MaxLineLength = 100,
			Colorize = true
		};
		var formatter = new SearchResultFormatter(settings);
		var pattern = new Regex("maxlen");
		var filePath = new FilePath("./maxlen.txt");
		var file = new FileResult(filePath, FileType.Text);
		const int lineNum = 1;
		const int matchStartIndex = 53;
		const int matchEndIndex = 59;
		const string line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
		var linesBeforeAfter = new List<string>();

		var searchResult = new SearchResult(pattern, file, lineNum, matchStartIndex,
			matchEndIndex, line, linesBeforeAfter, linesBeforeAfter);
		const string expectedPath = "./maxlen.txt";
		var expectedLine = "...89012345678901234567890123456789012345678901" +
		                   Color.Green +
		                   "maxlen" +
		                   Color.Reset +
		                   "89012345678901234567890123456789012345678901...";
		var expectedOutput = $"{expectedPath}: {lineNum}: [{matchStartIndex}:{matchEndIndex}]: {expectedLine}";

		var output = formatter.Format(searchResult);
		Assert.That(output, Is.EqualTo(expectedOutput));
	}

	[Test]
	public void SearchResultMultiLine_ToString_EqualsExpected()
	{
		var settings = new SearchSettings
		{
			Colorize = false
		};
		var formatter = new SearchResultFormatter(settings);
		var pattern = new Regex("Search");
		var filePath = new FilePath(Path.Join(CsSearchPath, "Searcher.cs"));
		var file = new FileResult(filePath, FileType.Code);
		const int lineNum = 10;
		const int matchStartIndex = 15;
		const int matchEndIndex = 23;
		const string line = "\tpublic class Searcher";
		var linesBefore = new List<string> { "namespace CsSearch", "{" };
		var linesAfter = new List<string> {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};
		var searchResult = new SearchResult(pattern, file, lineNum,
			matchStartIndex, matchEndIndex,
			line, linesBefore, linesAfter);
		var expandedSearchPath = FileUtil.ExpandPath(CsSearchPath);
		var expectedPath = Path.Join(expandedSearchPath, "Searcher.cs");
		var expectedOutput = string.Format(new string('=', 80) + "\n" +
		                                   "{0}: {1}: [{2}:{3}]\n" +
		                                   new string('-', 80) + "\n" +
		                                   "   8 | namespace CsSearch\n" +
		                                   "   9 | {{\n" +
		                                   "> 10 | 	public class Searcher\n" +
		                                   "  11 | 	{{\n" +
		                                   "  12 | 		private readonly FileTypes _fileTypes;\n",
			expectedPath, lineNum, matchStartIndex, matchEndIndex);
		var output = formatter.Format(searchResult);
		Assert.That(output, Is.EqualTo(expectedOutput));
	}

	[Test]
	public void SearchResultBinaryFile_ToString_EqualsExpected()
	{
		var settings = new SearchSettings();
		var formatter = new SearchResultFormatter(settings);
		var pattern = new Regex("Search");
		var filePath = new FilePath(Path.Join(CsSearchPath, "Searcher.exe"));
		var file = new FileResult(filePath, FileType.Binary);
		const int lineNum = 0;
		const int matchStartIndex = 0;
		const int matchEndIndex = 0;
		string? line = null;
		var searchResult = new SearchResult(pattern, file, lineNum,
			matchStartIndex, matchEndIndex, line);
		var expandedSearchPath = FileUtil.ExpandPath(CsSearchPath);
		var expectedPath = Path.Join(expandedSearchPath, "Searcher.exe");
		var expectedOutput = $"{expectedPath} matches at [0:0]";

		var output = formatter.Format(searchResult);
		Assert.That(output, Is.EqualTo(expectedOutput));
	}
}