using System.Collections.Generic;
using System.IO;
using System.Linq;
using NUnit.Framework;
using CsSearchLib;

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
			Assert.IsEmpty(settings.Paths);
			Assert.IsTrue(settings.PrintResults);
			Assert.IsFalse(settings.PrintUsage);
			Assert.IsFalse(settings.PrintVersion);
			Assert.IsTrue(settings.Recursive);
			Assert.IsFalse(settings.SearchArchives);
			Assert.IsFalse(settings.UniqueLines);
			Assert.IsFalse(settings.Verbose);
		}

		[Test]
		public void SettingsFromArgs_ValidArgs_HasArgValues()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Search", "." };
			var settings = _searchOptions.SettingsFromArgs(args);
			var startInfo = new DirectoryInfo(".");
			Assert.AreEqual(1, settings.Paths.Count);
			Assert.AreEqual(startInfo.ToString(), settings.Paths.First());
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
		
		[Test]
		public void SettingsFromJson_EqualsExpected()
		{
			var json = @"{
  ""path"": ""~/src/xsearch/"", 
  ""in-ext"": [""js"", ""ts""],
  ""out-dirpattern"": ""node_module"",
  ""out-filepattern"": [""temp""],
  ""searchpattern"": ""Searcher"",
  ""linesbefore"": 2,
  ""linesafter"": 2,
  ""debug"": true,
  ""allmatches"": false,
  ""includehidden"": false
}";
			var settings = new SearchSettings();
			SearchOptions.SettingsFromJson(json, settings);

			Assert.AreEqual(1, settings.Paths.Count);
			Assert.AreEqual("~/src/xsearch/", settings.Paths.First());

			Assert.AreEqual(2, settings.InExtensions.Count);
			Assert.True(settings.InExtensions.Contains(".js"));
			Assert.True(settings.InExtensions.Contains(".ts"));

			Assert.AreEqual(1, settings.OutDirPatterns.Count);
			Assert.AreEqual("node_module", settings.OutDirPatterns.First().ToString());

			Assert.AreEqual(1, settings.OutFilePatterns.Count);
			Assert.AreEqual("temp", settings.OutFilePatterns.First().ToString());

			Assert.AreEqual(1, settings.SearchPatterns.Count);
			Assert.AreEqual("Searcher", settings.SearchPatterns.First().ToString());

			Assert.AreEqual(2, settings.LinesBefore);
			Assert.AreEqual(2, settings.LinesAfter);

			Assert.True(settings.Debug);
			Assert.True(settings.FirstMatch);
			Assert.True(settings.ExcludeHidden);
		}
	}
}
