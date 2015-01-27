using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using NUnit.Framework;
using CsSearch;

namespace CsSearchTests
{
	[TestFixture]
	public class SearchOptionsTests
	{
		SearchOptions searchOptions = new SearchOptions();

		[Test]
		public void SettingsFromArgs_NoArgs_HasDefaultValues()
		{
			var settings = searchOptions.SettingsFromArgs(new List<string>());
			Assert.IsFalse(settings.ArchivesOnly);
			Assert.IsFalse(settings.Debug);
			Assert.IsFalse(settings.DoTiming);
			Assert.IsTrue(settings.ExcludeHidden);
			Assert.IsFalse(settings.FirstMatch);
			Assert.AreEqual(settings.LinesAfter, 0);
			Assert.AreEqual(settings.LinesBefore, 0);
			Assert.IsFalse(settings.ListDirs);
			Assert.IsFalse(settings.ListFiles);
			Assert.IsFalse(settings.ListLines);
			Assert.AreEqual(settings.MaxLineLength, 150);
			Assert.IsFalse(settings.MultiLineSearch);
			Assert.IsTrue(settings.PrintResults);
			Assert.IsFalse(settings.PrintUsage);
			Assert.IsFalse(settings.PrintVersion);
			Assert.IsTrue(settings.Recursive);
			Assert.IsFalse(settings.SearchArchives);
			Assert.AreEqual(settings.StartPath, null);
			Assert.IsFalse(settings.UniqueLines);
			Assert.IsFalse(settings.Verbose);
		}

		[Test]
		public void SettingsFromArgs_ValidArgs_HasArgValues()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Search", "." };
			var settings = searchOptions.SettingsFromArgs(args);
			var startFile = new FileInfo(".");
			Assert.AreEqual(settings.StartPath, startFile.FullName);
			Assert.AreEqual(settings.InExtensions.Count, 1);
			Assert.IsTrue(settings.InExtensions.Contains(".cs"));
			Assert.AreEqual(settings.SearchPatterns.Count, 1);
			Assert.IsTrue(settings.SearchPatterns.First().ToString() == "Search");
		}

		[Test]
		public void SettingsFromArgs_InValidArgs_ThrowsSearchException()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Search", ".", "-Q" };
			var ex = Assert.Throws<SearchException>(() => searchOptions.SettingsFromArgs(args));
			Assert.AreEqual(ex.Message, "Unknown option: Q");
		}
	}
}
