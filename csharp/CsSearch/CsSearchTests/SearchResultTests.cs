using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using CsSearch;
using NUnit.Framework;

namespace CsSearchTests
{
	[TestFixture]
	class SearchResultTests
	{
		string CsSearchPath = "~/src/xsearch/csharp/CsSearch/CsSearch";

		[Test]
		public void SearchResultSingleLine_ToString_EqualsExpected()
		{
			var settings = new SearchSettings();
			var pattern = new Regex("Search");
			var searchFile = new SearchFile(CsSearchPath, "Searcher.cs", FileType.Text);
			const int lineNum = 10;
			const int matchStartIndex = 15;
			const int matchEndIndex = 23;
			const string line = "\tpublic class Searcher\n";
			var searchResult = new SearchResult(pattern, searchFile, lineNum,
				matchStartIndex, matchEndIndex, line);
			var expectedPath = CsSearchPath + "/Searcher.cs";
			var expectedOutput = string.Format("{0}: {1}: [{2}:{3}]: {4}", expectedPath,
				lineNum, matchStartIndex, matchEndIndex, line.Trim());

			Assert.AreEqual(searchResult.ToString(settings), expectedOutput);
		}

		[Test]
		public void SearchResultMultiLine_ToString_EqualsExpected()
		{
			var settings = new SearchSettings();
			var pattern = new Regex("Search");
			var searchFile = new SearchFile(CsSearchPath, "Searcher.cs", FileType.Text);
			const int lineNum = 10;
			const int matchStartIndex = 15;
			const int matchEndIndex = 23;
			const string line = "\tpublic class Searcher";
			var linesBefore = new List<string> { "namespace CsSearch", "{" };
			var linesAfter = new List<string> {"\t{", "\t\tprivate readonly FileTypes _fileTypes;"};
			var searchResult = new SearchResult(pattern, searchFile, lineNum,
												matchStartIndex, matchEndIndex,
												line, linesBefore, linesAfter);
			var expectedPath = CsSearchPath + "/Searcher.cs";
			var expectedOutput = string.Format(new string('=', 80) + "\n" +
								 "{0}: {1}: [{2}:{3}]\n" +
								 new string('-', 80) + "\n" +
								 "   8 | namespace CsSearch\n" +
								 "   9 | {{\n" +
								 "> 10 | 	public class Searcher\n" +
								 "  11 | 	{{\n" +
								 "  12 | 		private readonly FileTypes _fileTypes;\n",
								 expectedPath, lineNum, matchStartIndex, matchEndIndex);
			Assert.AreEqual(searchResult.ToString(settings), expectedOutput);
		}

		[Test]
		public void SearchResultBinaryFile_ToString_EqualsExpected()
		{
			var settings = new SearchSettings();
			var pattern = new Regex("Search");
			var searchFile = new SearchFile(CsSearchPath, "Searcher.exe", FileType.Binary);
			const int lineNum = 0;
			const int matchStartIndex = 0;
			const int matchEndIndex = 0;
			string line = null;
			var searchResult = new SearchResult(pattern, searchFile, lineNum,
				matchStartIndex, matchEndIndex, line);
			var expectedPath = CsSearchPath + "/Searcher.exe";
			var expectedOutput = $"{expectedPath} matches at [0:0]";

			Assert.AreEqual(searchResult.ToString(settings), expectedOutput);
		}
	}
}
