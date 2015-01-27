using System.Collections.Generic;
using System.Text.RegularExpressions;
using CsSearch;
using NUnit.Framework;

namespace CsSearchTests
{
	[TestFixture]
	class SearchResultTests
	{
		[Test]
		public void SearchResultSingleLine_ToString_EqualsExpected()
		{
			var pattern = new Regex("Search");
			var searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
				"Searcher.cs", FileType.Text);
			var lineNum = 10;
			var matchStartIndex = 15;
			var matchEndIndex = 23;
			var line = "\tpublic class Searcher\n";
			var searchResult = new SearchResult(pattern, searchFile, lineNum,
				matchStartIndex, matchEndIndex, line);
			var expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
			var expectedOutput = string.Format("{0}: {1}: [{2}:{3}]: {4}", expectedPath,
				lineNum, matchStartIndex, matchEndIndex, line.Trim());

			Assert.AreEqual(searchResult.ToString(), expectedOutput);
		}

		[Test]
		public void SearchResultMultiLine_ToString_EqualsExpected()
		{
			var pattern = new Regex("Search");
			var searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch",
											"Searcher.cs", FileType.Text);
			var lineNum = 10;
			var matchStartIndex = 15;
			var matchEndIndex = 23;
			var line = "\tpublic class Searcher";
			var linesBefore = new List<string> { "namespace CsSearch", "{" };
			var linesAfter = new List<string> {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};

			;
			var searchResult = new SearchResult(pattern, searchFile, lineNum,
												matchStartIndex, matchEndIndex,
												line, linesBefore, linesAfter);
			var expectedPath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs";
			var expectedOutput = string.Format(new string('=', 80) + "\n" +
								 "{0}: {1}: [{2}:{3}]\n" +
								 new string('-', 80) + "\n" +
								 "  8  | namespace CsSearch\n" +
								 "  9  | {{\n" +
								 "> 10 | 	public class Searcher\n" +
								 "  11 | 	{{\n" +
								 "  12 | 		private readonly FileTypes _fileTypes;\n",
								 expectedPath, lineNum, matchStartIndex, matchEndIndex);
			Assert.AreEqual(searchResult.ToString(), expectedOutput);
		}
	}
}