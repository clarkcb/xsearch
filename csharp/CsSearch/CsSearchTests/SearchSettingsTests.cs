using System.Linq;
using NUnit.Framework;
using CsSearchLib;

namespace CsSearchTests;

[TestFixture]
public class SearchSettingsTests
{
	[Test]
	public void GetNewSearchSettings_NoModifications_HasDefaultValues()
	{
		var settings = new SearchSettings();
		Assert.That(settings.ArchivesOnly, Is.False);
		Assert.That(settings.Colorize);
		Assert.That(settings.Debug, Is.False);
		Assert.That(settings.FirstMatch, Is.False);
		Assert.That(settings.IncludeHidden, Is.False);
		Assert.That(settings.LinesAfter, Is.EqualTo(0));
		Assert.That(settings.LinesBefore, Is.EqualTo(0));
		Assert.That(settings.MaxLineLength, Is.EqualTo(150));
		Assert.That(settings.MultiLineSearch, Is.False);
		Assert.That(settings.PrintDirs, Is.False);
		Assert.That(settings.PrintFiles, Is.False);
		Assert.That(settings.PrintLines, Is.False);
		Assert.That(settings.PrintResults, Is.False);
		Assert.That(settings.PrintUsage, Is.False);
		Assert.That(settings.PrintVersion, Is.False);
		Assert.That(settings.Recursive);
		Assert.That(settings.SearchArchives, Is.False);
		Assert.That(settings.UniqueLines, Is.False);
		Assert.That(settings.Verbose, Is.False);
	}

	[Test]
	public void SearchSettings_AddExtensions_HasExtensions()
	{
		var settings = new SearchSettings();
		settings.AddInExtension("cs");
		Assert.That(settings.InExtensions.Count, Is.EqualTo(1));
		Assert.That(settings.InExtensions.Contains(".cs"));
		settings.AddInExtension("java,scala");
		Assert.That(settings.InExtensions.Count, Is.EqualTo(3));
		Assert.That(settings.InExtensions.Contains(".java"));
		Assert.That(settings.InExtensions.Contains(".scala"));
	}

	[Test]
	public void SearchSettings_AddPatterns_HasPatterns()
	{
		var settings = new SearchSettings();
		settings.AddSearchPattern("Search");
		Assert.That(settings.SearchPatterns.Count, Is.EqualTo(1));
		Assert.That(settings.SearchPatterns.First().ToString() == "Search");
	}

	[Test]
	public void SearchSettings_SetArchivesOnly_HasSearchArchives()
	{
		var settings = new SearchSettings {ArchivesOnly = true};
		Assert.That(settings.ArchivesOnly);
		Assert.That(settings.SearchArchives);
		Assert.That(settings.IncludeArchives);
	}

	[Test]
	public void SearchSettings_SetDebug_HasVerbose()
	{
		var settings = new SearchSettings {Debug = true};
		Assert.That(settings.Debug);
		Assert.That(settings.Verbose);
	}
}
