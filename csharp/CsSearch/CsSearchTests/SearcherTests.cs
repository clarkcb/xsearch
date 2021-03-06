using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using CsSearchLib;
using NUnit.Framework;

namespace CsSearchTests
{
	[TestFixture]
	class SearcherTests
	{
		private readonly FileTypes _fileTypes = new FileTypes();
		
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

		// /*************************************************************
		//  * IsSearchDirectory tests
		// *************************************************************/
		// [Test]
		// public void TestIsSearchDirectory_SingleDot_True()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	Assert.True(searcher.IsSearchDirectory(new DirectoryInfo(".")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_DoubleDot_True()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	Assert.True(searcher.IsSearchDirectory(new DirectoryInfo("..")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_IsHidden_False()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	Assert.False(searcher.IsSearchDirectory(new DirectoryInfo(".git")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_IsHiddenIncludeHidden_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.ExcludeHidden = false;
		// 	var searcher = new Searcher(settings);
		// 	Assert.True(searcher.IsSearchDirectory(new DirectoryInfo(".git")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_NoPatterns_True()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	Assert.True(searcher.IsSearchDirectory(new DirectoryInfo("/Users")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_MatchesInPattern_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInDirPattern("Search");
		// 	var searcher = new Searcher(settings);
		// 	Assert.True(searcher.IsSearchDirectory(new DirectoryInfo("CsSearch")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_MatchesOutPattern_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutDirPattern("Search");
		// 	var searcher = new Searcher(settings);
		// 	Assert.False(searcher.IsSearchDirectory(new DirectoryInfo("CsSearch")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_DoesNotMatchInPattern_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInDirPattern("SearchFiles");
		// 	var searcher = new Searcher(settings);
		// 	Assert.False(searcher.IsSearchDirectory(new DirectoryInfo("CsSearch")));
		// }

		// [Test]
		// public void TestIsSearchDirectory_DoesNotMatchOutPattern_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutDirPattern("SearchFiles");
		// 	var searcher = new Searcher(settings);
		// 	var dir = new DirectoryInfo("CsSearch");
		// 	Assert.True(searcher.IsSearchDirectory(dir));
		// }


		// /*************************************************************
		//  * IsSearchFile tests
		// *************************************************************/

		// [Test]
		// public void TestIsSearchFile_NoExtensionsNoPatterns_True()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_MatchesInExtension_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInExtension("cs");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_DoesNotMatchInExtension_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInExtension("java");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsSearchFile(sf));
		// }


		// [Test]
		// public void TestIsSearchFile_MatchesOutExtension_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutExtension("cs");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_DoesNotMatchOutExtension_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutExtension("java");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_MatchesInPattern_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInFilePattern("Search");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("Searcher.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_DoesNotMatchInPattern_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInFilePattern("Search");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_MatchesOutPattern_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutFilePattern("Search");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("Searcher.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsSearchFile(sf));
		// }

		// [Test]
		// public void TestIsSearchFile_DoesNotMatchOutPattern_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutFilePattern("Search");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsSearchFile(sf));
		// }


		// /*************************************************************
		//  * IsArchiveSearchFile tests
		// *************************************************************/

		// [Test]
		// public void TestIsArchiveSearchFile_NoExtensionsNoPatterns_True()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_MatchesInExtension_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInArchiveExtension("zip");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_DoesNotMatchInExtension_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInArchiveExtension("gz");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsArchiveSearchFile(sf));
		// }


		// [Test]
		// public void TestIsArchiveSearchFile_MatchesOutExtension_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutArchiveExtension("zip");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_DoesNotMatchOutExtension_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutArchiveExtension("gz");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_MatchesInPattern_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInArchiveFilePattern("arch");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_DoesNotMatchInPattern_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInArchiveFilePattern("archives");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_MatchesOutPattern_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutArchiveFilePattern("arch");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.IsArchiveSearchFile(sf));
		// }

		// [Test]
		// public void TestIsArchiveSearchFile_DoesNotMatchOutPattern_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutArchiveFilePattern("archives");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.IsArchiveSearchFile(sf));
		// }

		// /*************************************************************
		//  * FilterFile tests
		// *************************************************************/

		// [Test]
		// public void TestFilterFile_IsHidden_False()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo(".gitignore");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_IsHiddenIncludeHidden_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.ExcludeHidden = false;
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo(".gitignore");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_ArchiveNoSearchArchives_False()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_ArchiveSearchArchives_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.SearchArchives = true;
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_IsArchiveSearchFile_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.SearchArchives = true;
		// 	settings.AddInArchiveExtension("zip");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_NotIsArchiveSearchFile_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutExtension("zip");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_ArchiveFileArchivesOnly_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.ArchivesOnly = true;
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("archive.zip");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.FilterFile(sf));
		// }


		// [Test]
		// public void TestFilterFile_NoExtensionsNoPatterns_True()
		// {
		// 	var settings = GetSettings();
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_IsSearchFile_True()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddInExtension("cs");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.True(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_NotIsSearchFile_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.AddOutExtension("cs");
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.FilterFile(sf));
		// }

		// [Test]
		// public void TestFilterFile_NonArchiveFileArchivesOnly_False()
		// {
		// 	var settings = GetSettings();
		// 	settings.ArchivesOnly = true;
		// 	var searcher = new Searcher(settings);
		// 	var file = new FileInfo("FileUtil.cs");
		// 	var sf = new SearchFile(file, _fileTypes.GetFileType(file));
		// 	Assert.False(searcher.FilterFile(sf));
		// }

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
}
