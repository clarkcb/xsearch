using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using CsSearchLib;
using NUnit.Framework;

using CsFindLib;

namespace CsSearchTests;

[TestFixture]
public class SearchResultTests
{
	private const string CsSearchPath = "~/src/xsearch/csharp/CsSearch/CsSearch";

	public static SearchResult GetSearchResult(string pattern, string filePath, FileType fileType,
		int lineNum, int matchStartIndex, int matchEndIndex, string? line)
	{
		return GetSearchResult(pattern, filePath, fileType, lineNum, matchStartIndex, matchEndIndex,
			line, [], []);
	}
	
	public static SearchResult GetSearchResult(string pattern, string filePath, FileType fileType,
		int lineNum, int matchStartIndex, int matchEndIndex, string? line, List<string> linesBefore,
		List<string> linesAfter)
	{
		var file = new FileResult(filePath, fileType);
		return new SearchResult(new Regex(pattern), file, lineNum, matchStartIndex, matchEndIndex,
			line, linesBefore, linesAfter);
	}

	[Test]
	public void SearchResultSingleLine_EqualsExpected()
	{
		const string pattern = "Search";
		var filePath = Path.Join(CsSearchPath, "Searcher.cs");
		const int lineNum = 10;
		const int matchStartIndex = 15;
		const int matchEndIndex = 23;
		const string line = "\tpublic class Searcher\n";
		var searchResult = GetSearchResult(pattern, filePath, FileType.Code, lineNum, matchStartIndex,
			matchEndIndex, line);

		Assert.That(searchResult.SearchPattern.ToString(), Is.EqualTo(pattern));
		Assert.That(searchResult.File!.FilePath.Path, Is.EqualTo(filePath));
		Assert.That(searchResult.LineNum, Is.EqualTo(lineNum));
		Assert.That(searchResult.MatchStartIndex, Is.EqualTo(matchStartIndex));
		Assert.That(searchResult.MatchEndIndex, Is.EqualTo(matchEndIndex));
		Assert.That(searchResult.Line, Is.EqualTo(line));
	}

	[Test]
	public void SearchResultMultiLine_EqualsExpected()
	{
		const string pattern = "Search";
		var filePath = Path.Join(CsSearchPath, "Searcher.cs");
		const int lineNum = 10;
		const int matchStartIndex = 15;
		const int matchEndIndex = 23;
		const string line = "\tpublic class Searcher\n";
		var linesBefore = new List<string> { "namespace CsSearch", "{" };
		var linesAfter = new List<string> {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};
		var searchResult = GetSearchResult(pattern, filePath, FileType.Code, lineNum, matchStartIndex,
			matchEndIndex, line, linesBefore, linesAfter);

		Assert.That(searchResult.SearchPattern.ToString(), Is.EqualTo(pattern));
		Assert.That(searchResult.File!.FilePath.Path, Is.EqualTo(filePath));
		Assert.That(searchResult.LineNum, Is.EqualTo(lineNum));
		Assert.That(searchResult.MatchStartIndex, Is.EqualTo(matchStartIndex));
		Assert.That(searchResult.MatchEndIndex, Is.EqualTo(matchEndIndex));
		Assert.That(searchResult.Line, Is.EqualTo(line));
		Assert.That(searchResult.LinesBefore.Count, Is.EqualTo(2));
		Assert.That(searchResult.LinesAfter.Count, Is.EqualTo(2));
	}
}