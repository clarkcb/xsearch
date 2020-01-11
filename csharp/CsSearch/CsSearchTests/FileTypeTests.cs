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
		public void GetFileType_CodeFile_FileTypeCode()
		{
			var codeFile = new FileInfo("code.cs");
			Assert.AreEqual(_fileTypes.GetFileType(codeFile), FileType.Code);
		}

		[Test]
		public void GetFileType_TextFile_FileTypeText()
		{
			var textFile = new FileInfo("text.txt");
			Assert.AreEqual(_fileTypes.GetFileType(textFile), FileType.Text);
		}

		[Test]
		public void GetFileType_XmlFile_FileTypeXml()
		{
			var xmlFile = new FileInfo("markup.xml");
			Assert.AreEqual(_fileTypes.GetFileType(xmlFile), FileType.Xml);
		}

		[Test]
		public void GetFileType_UnknownFile_FileTypeUnknown()
		{
			var unknownFile = new FileInfo("unknown.xyz");
			Assert.AreEqual(_fileTypes.GetFileType(unknownFile), FileType.Unknown);
		}

		[Test]
		public void IsArchiveFile_ArchiveFile_True()
		{
			var archiveFile = new FileInfo("archive.zip");
			Assert.IsTrue(_fileTypes.IsArchiveFile(archiveFile));
		}

		[Test]
		public void IsBinaryFile_BinaryFile_True()
		{
			var binaryFile = new FileInfo("binary.exe");
			Assert.IsTrue(_fileTypes.IsBinaryFile(binaryFile));
		}

		[Test]
		public void IsCodeFile_CodeFile_True()
		{
			var codeFile = new FileInfo("code.cs");
			Assert.IsTrue(_fileTypes.IsCodeFile(codeFile));
		}

		[Test]
		public void IsTextFile_TextFile_True()
		{
			var textFile = new FileInfo("text.txt");
			Assert.IsTrue(_fileTypes.IsTextFile(textFile));
		}

		[Test]
		public void IsXmlFile_XmlFile_True()
		{
			var xmlFile = new FileInfo("markup.xml");
			Assert.IsTrue(_fileTypes.IsXmlFile(xmlFile));
		}

		[Test]
		public void IsSearchableFile_XmlFile_True()
		{
			var xmlFile = new FileInfo("markup.xml");
			Assert.IsTrue(_fileTypes.IsSearchableFile(xmlFile));
		}
	}
}
