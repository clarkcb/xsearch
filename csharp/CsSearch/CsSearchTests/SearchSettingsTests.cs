using System.Linq;
using NUnit.Framework;
using CsSearch;

namespace CsSearchTests
{
	[TestFixture]
	public class SearchSettingsTests
	{
		[Test]
		public void GetNewSearchSettings_NoModifications_HasDefaultValues()
		{
			var settings = new SearchSettings();
			Assert.IsFalse(settings.ArchivesOnly);
			Assert.IsFalse(settings.Debug);
			Assert.IsTrue(settings.ExcludeHidden);
			Assert.IsFalse(settings.FirstMatch);
			Assert.AreEqual(settings.LinesAfter, 0);
			Assert.AreEqual(settings.LinesBefore, 0);
			Assert.IsFalse(settings.ListDirs);
			Assert.IsFalse(settings.ListFiles);
			Assert.IsFalse(settings.ListLines);
			Assert.AreEqual(settings.MaxLineLength, 150);
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
			Assert.AreEqual(settings.InExtensions.Count, 1);
			Assert.IsTrue(settings.InExtensions.Contains(".cs"));
			settings.AddInExtension("java,scala");
			Assert.AreEqual(settings.InExtensions.Count, 3);
			Assert.IsTrue(settings.InExtensions.Contains(".java"));
			Assert.IsTrue(settings.InExtensions.Contains(".scala"));
		}

		[Test]
		public void SearchSettings_AddPatterns_HasPatterns()
		{
			var settings = new SearchSettings();
			settings.AddSearchPattern("Search");
			Assert.AreEqual(settings.SearchPatterns.Count, 1);
			Assert.IsTrue(settings.SearchPatterns.First().ToString() == "Search");
		}

		[Test]
		public void SearchSettings_SetArchivesOnly_HasSearchArchives()
		{
			var settings = new SearchSettings();
			settings.SetArchivesOnly();
			Assert.IsTrue(settings.ArchivesOnly);
			Assert.IsTrue(settings.SearchArchives);
		}

		[Test]
		public void SearchSettings_SetDebug_HasVerbose()
		{
			var settings = new SearchSettings();
			settings.SetDebug();
			Assert.IsTrue(settings.Debug);
			Assert.IsTrue(settings.Verbose);
		}
	}
}
