using System.Collections.Generic;
using System.Linq;
using CsFindLib;
using CsSearchLib;
using NUnit.Framework;

namespace CsSearchTests;

[TestFixture]
public class SearchOptionsTests
{
	private readonly SearchOptions _searchOptions = new();

	[Test]
	public void SettingsFromArgs_NoArgs_HasDefaultValues()
	{
		var settings = _searchOptions.SettingsFromArgs(new List<string>());
		Assert.That(settings.ArchivesOnly, Is.False);
		Assert.That(settings.Colorize);
		Assert.That(settings.Debug, Is.False);
		Assert.That(settings.FirstMatch, Is.False);
		Assert.That(settings.IncludeArchives, Is.False);
		Assert.That(settings.IncludeHidden, Is.False);
		Assert.That(settings.LinesAfter, Is.EqualTo(0));
		Assert.That(settings.LinesBefore, Is.EqualTo(0));
		Assert.That(settings.MaxLineLength, Is.EqualTo(150));
		Assert.That(settings.MultiLineSearch, Is.False);
		Assert.That(settings.Paths.Count, Is.EqualTo(0));
		Assert.That(settings.PrintDirs, Is.False);
		Assert.That(settings.PrintFiles, Is.False);
		Assert.That(settings.PrintLines, Is.False);
		Assert.That(settings.PrintResults);
		Assert.That(settings.PrintUsage, Is.False);
		Assert.That(settings.PrintVersion, Is.False);
		Assert.That(settings.Recursive);
		Assert.That(settings.SearchArchives, Is.False);
		Assert.That(settings.UniqueLines, Is.False);
		Assert.That(settings.Verbose, Is.False);
	}

	[Test]
	public void SettingsFromArgs_ValidArgs_HasArgValues()
	{
		var args = new List<string>() { "-x", "cs", "-s", "Search", "." };
		var settings = _searchOptions.SettingsFromArgs(args);
		var startPath = new FilePath(".");
		Assert.That(settings.Paths.Count, Is.EqualTo(1));
		Assert.That(settings.Paths.Count, Is.EqualTo(1));
		Assert.That(settings.Paths.First().Path, Is.EqualTo(startPath.Path));
		Assert.That(settings.InExtensions.Count, Is.EqualTo(1));
		Assert.That(settings.InExtensions.Contains(".cs"));
		Assert.That(settings.SearchPatterns.Count, Is.EqualTo(1));
		Assert.That(settings.SearchPatterns.First().ToString() == "Search");
	}

	[Test]
	public void SettingsFromArgs_ArchivesOnly_SearchArchives()
	{
		var args = new List<string>() { "-x", "cs", "-s", "Search", "--archivesonly", "." };
		var settings = _searchOptions.SettingsFromArgs(args);
		Assert.That(settings.ArchivesOnly);
		Assert.That(settings.SearchArchives);
	}

	[Test]
	public void SettingsFromArgs_InValidArgs_ThrowsSearchException()
	{
		var args = new List<string>() { "-x", "cs", "-s", "Search", ".", "-Q" };
		var ex = Assert.Throws<SearchException>(() => _searchOptions.SettingsFromArgs(args));
		Assert.That(ex?.Message, Is.EqualTo("Invalid option: Q"));
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
  ""includehidden"": true
}";
		var options = new SearchOptions();
		var settings = new SearchSettings();
		options.UpdateSettingsFromJson(settings, json);
		var startPath = new FilePath("~/src/xsearch/");

		Assert.That(settings.Paths.Count, Is.EqualTo(1));
		Assert.That(settings.Paths.First().Path, Is.EqualTo(startPath.Path));

		Assert.That(settings.InExtensions.Count, Is.EqualTo(2));
		Assert.That(settings.InExtensions.Contains(".js"));
		Assert.That(settings.InExtensions.Contains(".ts"));

		Assert.That(settings.OutDirPatterns.Count, Is.EqualTo(1));
		Assert.That(settings.OutDirPatterns.First().ToString(), Is.EqualTo("node_module"));

		Assert.That(settings.OutFilePatterns.Count, Is.EqualTo(1));
		Assert.That(settings.OutFilePatterns.First().ToString(), Is.EqualTo("temp"));

		Assert.That(settings.SearchPatterns.Count, Is.EqualTo(1));
		Assert.That(settings.SearchPatterns.First().ToString(), Is.EqualTo("Searcher"));

		Assert.That(settings.LinesBefore, Is.EqualTo(2));
		Assert.That(settings.LinesAfter, Is.EqualTo(2));

		Assert.That(settings.Debug);
		Assert.That(settings.FirstMatch);
		Assert.That(settings.IncludeHidden);
	}
}
