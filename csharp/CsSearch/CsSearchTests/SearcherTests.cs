using System;
using System.Collections.Generic;
using System.Linq;
using CsSearchLib;
using NUnit.Framework;

namespace CsSearchTests;

[TestFixture]
class SearcherTests
{
	private static string GetTestFileContent()
	{
		return EmbeddedTestResource.GetResourceFileContents("CsSearchTests.Resources.testFile2.txt");
	}

	public static IEnumerable<string> GetTestFileLines()
	{
		var testFile2Contents = GetTestFileContent();
		foreach (var line in testFile2Contents.Split(["\n", "\r"], StringSplitOptions.None))
		{
			yield return line;
		}
	}

	private static SearchSettings GetSettings()
	{
		// var settings = new SearchSettings {StartPath = "."};
		var settings = new SearchSettings();
		settings.AddPath(".");
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

		Assert.That(results.Count == 2);

		var firstResult = results[0];
		const int expectedFirstLineNum = 30;
		Assert.That(firstResult.LineNum, Is.EqualTo(expectedFirstLineNum));
		const int expectedFirstMatchStartIndex = 3;
		Assert.That(firstResult.MatchStartIndex, Is.EqualTo(expectedFirstMatchStartIndex));
		const int expectedFirstMatchEndIndex = 11;
		Assert.That(firstResult.MatchEndIndex, Is.EqualTo(expectedFirstMatchEndIndex));

		var secondResult = results[1];
		const int expectedSecondLineNum = 36;
		Assert.That(secondResult.LineNum, Is.EqualTo(expectedSecondLineNum));
		const int expectedSecondMatchStartIndex = 24;
		Assert.That(secondResult.MatchStartIndex, Is.EqualTo(expectedSecondMatchStartIndex));
		const int expectedSecondMatchEndIndex = 32;
		Assert.That(secondResult.MatchEndIndex, Is.EqualTo(expectedSecondMatchEndIndex));
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

		Assert.That(results.Count == 2);

		var firstResult = results[0];
		const int expectedFirstLineNum = 30;
		Assert.That(firstResult.LineNum, Is.EqualTo(expectedFirstLineNum));
		const int expectedFirstMatchStartIndex = 3;
		Assert.That(firstResult.MatchStartIndex, Is.EqualTo(expectedFirstMatchStartIndex));
		const int expectedFirstMatchEndIndex = 11;
		Assert.That(firstResult.MatchEndIndex, Is.EqualTo(expectedFirstMatchEndIndex));

		var secondResult = results[1];
		const int expectedSecondLineNum = 36;
		Assert.That(secondResult.LineNum, Is.EqualTo(expectedSecondLineNum));
		const int expectedSecondMatchStartIndex = 24;
		Assert.That(secondResult.MatchStartIndex, Is.EqualTo(expectedSecondMatchStartIndex));
		const int expectedSecondMatchEndIndex = 32;
		Assert.That(secondResult.MatchEndIndex, Is.EqualTo(expectedSecondMatchEndIndex));
	}
}