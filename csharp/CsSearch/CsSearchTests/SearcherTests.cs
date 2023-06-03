using System;
using System.Collections.Generic;
using System.Linq;
using CsFindLib;
using CsSearchLib;
using NUnit.Framework;

namespace CsSearchTests;

[TestFixture]
class SearcherTests
{
	private readonly FileTypes _fileTypes = new();
		
	private static string GetTestFileContent()
	{
		return EmbeddedTestResource.GetResourceFileContents("CsSearchTests.Resources.testFile2.txt");
	}

	public static IEnumerable<string> GetTestFileLines()
	{
		var testFile2Contents = GetTestFileContent();
		foreach (var line in testFile2Contents.Split(new[] { "\n", "\r" }, StringSplitOptions.None))
		{
			yield return line;
		}
	}

	private static SearchSettings GetSettings()
	{
		// var settings = new SearchSettings {StartPath = "."};
		var settings = new SearchSettings();
		settings.Paths.Add(".");
		settings.AddSearchPattern("Searcher");
		return settings;
	}

	/*************************************************************
	 * SearchTextReaderLines test
	 *************************************************************/
	[Test]
	public void TestSearchTextReaderLines()
	{
		var settings = GetSettings();
		var searcher = new Searcher(settings);
		var enumerableLines = GetTestFileLines();
		var results = searcher.SearchLines(enumerableLines).ToList();

		Assert.True(results.Count == 2);

		var firstResult = results[0];
		const int expectedFirstLineNum = 29;
		Assert.AreEqual(expectedFirstLineNum, firstResult.LineNum);
		const int expectedFirstMatchStartIndex = 3;
		Assert.AreEqual(expectedFirstMatchStartIndex, firstResult.MatchStartIndex);
		const int expectedFirstMatchEndIndex = 11;
		Assert.AreEqual(expectedFirstMatchEndIndex, firstResult.MatchEndIndex);

		var secondResult = results[1];
		const int expectedSecondLineNum = 35;
		Assert.AreEqual(expectedSecondLineNum, secondResult.LineNum);
		const int expectedSecondMatchStartIndex = 24;
		Assert.AreEqual(expectedSecondMatchStartIndex, secondResult.MatchStartIndex);
		const int expectedSecondMatchEndIndex = 32;
		Assert.AreEqual(expectedSecondMatchEndIndex, secondResult.MatchEndIndex);
	}

	/*************************************************************
	 * SearchMultiLineString test
	 *************************************************************/
	[Test]
	public void TestSearchMultiLineString()
	{
		var settings = GetSettings();
		var searcher = new Searcher(settings);
		var contents = GetTestFileContent();
		var results = searcher.SearchContents(contents).ToList();

		Assert.True(results.Count == 2);

		var firstResult = results[0];
		const int expectedFirstLineNum = 29;
		Assert.AreEqual(expectedFirstLineNum, firstResult.LineNum);
		const int expectedFirstMatchStartIndex = 3;
		Assert.AreEqual(expectedFirstMatchStartIndex, firstResult.MatchStartIndex);
		const int expectedFirstMatchEndIndex = 11;
		Assert.AreEqual(expectedFirstMatchEndIndex, firstResult.MatchEndIndex);

		var secondResult = results[1];
		const int expectedSecondLineNum = 35;
		Assert.AreEqual(expectedSecondLineNum, secondResult.LineNum);
		const int expectedSecondMatchStartIndex = 24;
		Assert.AreEqual(expectedSecondMatchStartIndex, secondResult.MatchStartIndex);
		const int expectedSecondMatchEndIndex = 32;
		Assert.AreEqual(expectedSecondMatchEndIndex, secondResult.MatchEndIndex);
	}
}