using System;
using System.IO;
using NUnit.Framework;
using CsSearch;

namespace CsSearchTests
{
	[TestFixture]
	class FileUtilTests
	{
		[Test]
		public void IsHiddenFile_StartsWithDot_IsHidden()
		{
			var hiddenFile = new FileInfo(".FileUtilTests.cs");
			Assert.IsTrue(FileUtil.IsHiddenFile(hiddenFile));
		}

		[Test]
		public void IsHiddenFile_NotStartsWithDot_NotIsHidden()
		{
			var hiddenFile = new FileInfo("FileUtilTests.cs");
			Assert.IsFalse(FileUtil.IsHiddenFile(hiddenFile));
		}

		[Test]
		public void NormalizePath_NoTrailingSlash_UnchangedPath()
		{
			var path = "~/src/git/xsearch";
			Assert.AreEqual(FileUtil.NormalizePath(path), path);
		}

		[Test]
		public void NormalizePath_TrailingSlash_TrimmedPath()
		{
			var path = "~/src/git/xsearch/";
			Assert.AreEqual(FileUtil.NormalizePath(path), "~/src/git/xsearch");
		}

		[Test]
		public void NormalizePath_TrailingBackSlash_TrimmedPath()
		{
			var path = @"C:\src\git\xsearch\";
			Assert.AreEqual(FileUtil.NormalizePath(path), @"C:\src\git\xsearch");
		}

		[Test]
		public void JoinPath_NoTrailingSlash_EqualsExpected()
		{
			var path = "~/src/git/xsearch/csharp/CsSearch/CsSearchTests";
			var filename = "FileUtilTests.cs";
			var pathAndFile = path + "/" + filename;
			Assert.AreEqual(FileUtil.JoinPath(path, filename), pathAndFile);
		}

		[Test]
		public void JoinPath_TrailingSlash_EqualsExpected()
		{
			var path = "~/src/git/xsearch/csharp/CsSearch/CsSearchTests/";
			var filename = "FileUtilTests.cs";
			var pathAndFile = path + filename;
			Assert.AreEqual(FileUtil.JoinPath(path, filename), pathAndFile);
		}

		[Test]
		public void JoinPath_NoTrailingBackSlash_EqualsExpected()
		{
			var path = @"C:\src\git\xsearch";
			var filename = "FileUtilTests.cs";
			var pathAndFile = path + "\\" + filename;
			Assert.AreEqual(FileUtil.JoinPath(path, filename), pathAndFile);
		}

		[Test]
		public void JoinPath_TrailingBackSlash_EqualsExpected()
		{
			var path = @"C:\src\git\xsearch\";
			var filename = "FileUtilTests.cs";
			var pathAndFile = path + filename;
			Assert.AreEqual(FileUtil.JoinPath(path, filename), pathAndFile);
		}

		[Test]
		public void JoinPath_NoSlashes_EqualsExpected()
		{
			var path = "CsSearchTests";
			var filename = "FileUtilTests.cs";
			var pathAndFile = path + "/" + filename;
			Assert.AreEqual(FileUtil.JoinPath(path, filename), pathAndFile);
		}
	}
}
