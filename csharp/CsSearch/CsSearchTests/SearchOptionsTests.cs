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
		private readonly SearchOptions _searchOptions = new SearchOptions();

		[Test]
		public void SettingsFromArgs_NoArgs_HasDefaultValues()
		{
			var settings = _searchOptions.SettingsFromArgs(new List<string>());
			Assert.IsFalse(settings.ArchivesOnly);
			Assert.IsTrue(settings.Colorize);
			Assert.IsFalse(settings.Debug);
			Assert.IsTrue(settings.ExcludeHidden);
			Assert.IsFalse(settings.FirstMatch);
			Assert.AreEqual(0, settings.LinesAfter);
			Assert.AreEqual(0, settings.LinesBefore);
			Assert.IsFalse(settings.ListDirs);
			Assert.IsFalse(settings.ListFiles);
			Assert.IsFalse(settings.ListLines);
			Assert.AreEqual(150, settings.MaxLineLength);
			Assert.IsFalse(settings.MultiLineSearch);
			Assert.IsTrue(settings.PrintResults);
			Assert.IsFalse(settings.PrintUsage);
			Assert.IsFalse(settings.PrintVersion);
			Assert.IsTrue(settings.Recursive);
			Assert.IsFalse(settings.SearchArchives);
			Assert.AreEqual(null, settings.StartPath);
			Assert.IsFalse(settings.UniqueLines);
			Assert.IsFalse(settings.Verbose);
		}

		[Test]
		public void SettingsFromArgs_ValidArgs_HasArgValues()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Search", "." };
			var settings = _searchOptions.SettingsFromArgs(args);
			var startInfo = new DirectoryInfo(".");
			Assert.AreEqual(startInfo.ToString(), settings.StartPath);
			Assert.AreEqual(1, settings.InExtensions.Count);
			Assert.IsTrue(settings.InExtensions.Contains(".cs"));
			Assert.AreEqual(1, settings.SearchPatterns.Count);
			Assert.IsTrue(settings.SearchPatterns.First().ToString() == "Search");
		}

		[Test]
		public void SettingsFromArgs_InValidArgs_ThrowsSearchException()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Search", ".", "-Q" };
			var ex = Assert.Throws<SearchException>(() => _searchOptions.SettingsFromArgs(args));
			Assert.AreEqual("Invalid option: Q", ex.Message);
		}
	}
}
