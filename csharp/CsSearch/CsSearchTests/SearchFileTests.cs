using NUnit.Framework;
using CsSearch;

namespace CsSearchTests
{
	[TestFixture]
	class SearchFileTests
	{
		[Test]
		public void SearchFile_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch", "Searcher.cs", FileType.Text);
			Assert.AreEqual(searchFile.ToString(), "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs");
		}

		[Test]
		public void SearchFileTrailingSlash_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile("~/src/git/xsearch/csharp/CsSearch/CsSearch/", "Searcher.cs", FileType.Text);
			Assert.AreEqual(searchFile.ToString(), "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs");
		}

		[Test]
		public void SearchFileBackSlashes_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile(@"C:\src\git\xsearch\csharp\CsSearch\CsSearch", "Searcher.cs", FileType.Text);
			Assert.AreEqual(searchFile.ToString(), @"C:\src\git\xsearch\csharp\CsSearch\CsSearch\Searcher.cs");
		}

		[Test]
		public void SearchFileBackSlashesTrailingSlash_ToString_EqualsExpected()
		{
			var searchFile = new SearchFile(@"C:\src\git\xsearch\csharp\CsSearch\CsSearch\", "Searcher.cs", FileType.Text);
			Assert.AreEqual(searchFile.ToString(), @"C:\src\git\xsearch\csharp\CsSearch\CsSearch\Searcher.cs");
		}
	}
}
