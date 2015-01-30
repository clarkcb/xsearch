using System;
using System.IO;
using NUnit.Framework;
using CsSearch;

namespace CsSearchTests
{
	[TestFixture]
	class FileUtilTests
	{
		/*************************************************************
		 * GetRelativePath tests
		*************************************************************/
		[Test]
		public void GetRelativePath_PathWithCurrentDirectory_RelativePath()
		{
			string path = Environment.CurrentDirectory + "/rest/of/path/";
			Assert.AreEqual(FileUtil.GetRelativePath(path), "./rest/of/path/");
		}

		[Test]
		public void GetRelativePath_PathWithoutCurrentDirectory_FullPath()
		{
			string path = "/a/full/path/by/itself/";
			Assert.AreEqual(FileUtil.GetRelativePath(path), path);
		}

		[Test]
		public void GetRelativePath_RelativePath_Unchanged()
		{
			string path = "./a/relative/path/";
			Assert.AreEqual(FileUtil.GetRelativePath(path), path);
		}

		/*************************************************************
		 * IsDirectory tests
		*************************************************************/
		[Test]
		public void IsDirectory_IsSingleDot_IsDirectory()
		{
			const string dotDir = ".";
			Assert.IsTrue(FileUtil.IsDirectory(dotDir));
		}

		[Test]
		public void IsDirectory_IsDoubleDot_IsDirectory()
		{
			const string dotDir = "..";
			Assert.IsTrue(FileUtil.IsDirectory(dotDir));
		}

		[Test]
		public void IsDirectory_InvalidPath_NotIsDirectory()
		{
			const string invalidDir = "/this/path/is/invalid/ZZZ";
			Assert.IsFalse(FileUtil.IsDirectory(invalidDir));
		}

		/*************************************************************
		 * IsDotDir tests
		*************************************************************/
		[Test]
		public void IsDotDir_IsSingleDot_IsDotDir()
		{
			const string dotDir = ".";
			Assert.IsTrue(FileUtil.IsDotDir(dotDir));
		}

		[Test]
		public void IsDotDir_IsSingleDotWithTrailingSlash_IsDotDir()
		{
			const string dotDir = "./";
			Assert.IsTrue(FileUtil.IsDotDir(dotDir));
		}

		[Test]
		public void IsDotDir_IsDoubleDot_IsDotDir()
		{
			const string dotDir = "..";
			Assert.IsTrue(FileUtil.IsDotDir(dotDir));
		}

		[Test]
		public void IsDotDir_IsDoubleDotWithTrailingSlash_IsDotDir()
		{
			const string dotDir = "../";
			Assert.IsTrue(FileUtil.IsDotDir(dotDir));
		}

		[Test]
		public void IsDotDir_IsNotDotDir_IsNotDotDir()
		{
			const string nonDotDir = "~/path";
			Assert.IsFalse(FileUtil.IsDotDir(nonDotDir));
		}

		/*************************************************************
		 * IsHidden tests
		*************************************************************/
		[Test]
		public void IsHidden_StartsWithDot_IsHidden()
		{
			var hiddenFile = new FileInfo(".FileUtilTests.cs");
			Assert.IsTrue(FileUtil.IsHidden(hiddenFile));
		}

		[Test]
		public void IsHidden_NotStartsWithDot_NotIsHidden()
		{
			var hiddenFile = new FileInfo("FileUtilTests.cs");
			Assert.IsFalse(FileUtil.IsHidden(hiddenFile));
		}

		[Test]
		public void IsHidden_SingleDot_NotIsHidden()
		{
			var dotDir = new DirectoryInfo(".");
			Assert.IsFalse(FileUtil.IsHidden(dotDir));
		}

		[Test]
		public void IsHidden_DoubleDot_NotIsHidden()
		{
			var dotDir = new DirectoryInfo("..");
			Assert.IsFalse(FileUtil.IsHidden(dotDir));
		}

		/*************************************************************
		 * NormalizePath tests
		*************************************************************/
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

		/*************************************************************
		 * JoinPath tests
		*************************************************************/
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
