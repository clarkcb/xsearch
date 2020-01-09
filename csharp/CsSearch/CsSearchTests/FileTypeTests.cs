using System.IO;
using NUnit.Framework;
using CsSearch;

namespace CsSearchTests
{
	[TestFixture]
	public class FileTypesTests
	{
		private readonly FileTypes _fileTypes = new FileTypes();

		[Test]
		public void GetFileType_ArchiveFile_FileTypeArchive()
		{
			var archiveFile = new FileInfo("archive.zip");
			Assert.AreEqual(_fileTypes.GetFileType(archiveFile), FileType.Archive);
		}

		[Test]
		public void GetFileType_BinaryFile_FileTypeBinary()
		{
			var binaryFile = new FileInfo("binary.exe");
			Assert.AreEqual(_fileTypes.GetFileType(binaryFile), FileType.Binary);
		}

		[Test]
		public void GetFileType_TextFile_FileTypeText()
		{
			var testFile = new FileInfo("text.txt");
			Assert.AreEqual(_fileTypes.GetFileType(testFile), FileType.Text);
		}

		[Test]
		public void GetFileType_UnknownFile_FileTypeUnknown()
		{
			var unknownFile = new FileInfo("unknown.xyz");
			Assert.AreEqual(_fileTypes.GetFileType(unknownFile), FileType.Unknown);
		}

	}
}
