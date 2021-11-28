using NUnit.Framework;
using CsSearchLib;

namespace CsSearchTests
{
	[TestFixture]
	class SearchFileTests
	{
		private const string CsSearchPath = "~/src/xsearch/csharp/CsSearch/CsSearch";
		private const string WinCsSearchPath = @"C:\src\git\xsearch\csharp\CsSearch\CsSearch";

		[Test]
		public void SearchFile_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile(CsSearchPath, "Searcher.cs", FileType.Text);
			Assert.AreEqual(CsSearchPath + "/Searcher.cs", searchFile.ToString());
		}

		[Test]
		public void SearchFileTrailingSlash_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile(CsSearchPath + "/", "Searcher.cs", FileType.Text);
			Assert.AreEqual(CsSearchPath + "/Searcher.cs", searchFile.ToString());
		}

		[Test]
		public void SearchFileBackSlashes_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile(WinCsSearchPath, "Searcher.cs", FileType.Text);
			Assert.AreEqual(WinCsSearchPath + @"\Searcher.cs", searchFile.ToString());
		}

		[Test]
		public void SearchFileBackSlashesTrailingSlash_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile(WinCsSearchPath + @"\", "Searcher.cs", FileType.Text);
			Assert.AreEqual(WinCsSearchPath + @"\Searcher.cs", searchFile.ToString());
		}
	}
}
