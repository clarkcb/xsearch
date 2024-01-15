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
		Assert.IsFalse(settings.ArchivesOnly);
		Assert.IsTrue(settings.Colorize);
		Assert.IsFalse(settings.Debug);
		Assert.IsFalse(settings.FirstMatch);
		Assert.IsFalse(settings.IncludeHidden);
		Assert.AreEqual(0, settings.LinesAfter);
		Assert.AreEqual(0, settings.LinesBefore);
		Assert.IsFalse(settings.ListDirs);
		Assert.IsFalse(settings.ListFiles);
		Assert.IsFalse(settings.ListLines);
		Assert.AreEqual(150, settings.MaxLineLength);
		Assert.IsFalse(settings.MultiLineSearch);
		Assert.IsFalse(settings.PrintResults);
		Assert.IsFalse(settings.PrintUsage);
		Assert.IsFalse(settings.PrintVersion);
		Assert.IsTrue(settings.Recursive);
		Assert.IsFalse(settings.SearchArchives);
		Assert.IsFalse(settings.UniqueLines);
		Assert.IsFalse(settings.Verbose);
	}

	[Test]
	public void SearchSettings_AddExtensions_HasExtensions()
	{
		var settings = new SearchSettings();
		settings.AddInExtension("cs");
		Assert.AreEqual(1, settings.InExtensions.Count);
		Assert.IsTrue(settings.InExtensions.Contains(".cs"));
		settings.AddInExtension("java,scala");
		Assert.AreEqual(3, settings.InExtensions.Count);
		Assert.IsTrue(settings.InExtensions.Contains(".java"));
		Assert.IsTrue(settings.InExtensions.Contains(".scala"));
	}

	[Test]
	public void SearchSettings_AddPatterns_HasPatterns()
	{
		var settings = new SearchSettings();
		settings.AddSearchPattern("Search");
		Assert.AreEqual(1, settings.SearchPatterns.Count);
		Assert.IsTrue(settings.SearchPatterns.First().ToString() == "Search");
	}

	[Test]
	public void SearchSettings_SetArchivesOnly_HasSearchArchives()
	{
		var settings = new SearchSettings {ArchivesOnly = true};
		Assert.IsTrue(settings.ArchivesOnly);
		Assert.IsTrue(settings.SearchArchives);
		Assert.IsTrue(settings.IncludeArchives);
	}

	[Test]
	public void SearchSettings_SetDebug_HasVerbose()
	{
		var settings = new SearchSettings {Debug = true};
		Assert.IsTrue(settings.Debug);
		Assert.IsTrue(settings.Verbose);
	}
}