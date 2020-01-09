using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;

namespace CsSearch
{
	public enum FileType
	{
		Unknown,
		Archive,
		Binary,
		Code,
		Text,
		Xml
	};

	public class FileTypes
	{
		public readonly ISet<string> CurrentAndParentDirs = new HashSet<string> {".", ".."};

		private readonly string _fileTypesResource;
		private readonly IDictionary<string, ISet<string>> _fileTypesDictionary;

		private const string archive = "archive";
		private const string binary = "binary";
		private const string code = "code";
		private const string searchable = "searchable";
		private const string text = "text";
		private const string xml = "xml";

		public FileTypes()
		{
			_fileTypesResource = EmbeddedResource.GetResourceFileContents("CsSearch.Resources.filetypes.xml");
			_fileTypesDictionary = new Dictionary<string, ISet<string>>();
			PopulateFileTypes();
		}

		private void PopulateFileTypes()
		{
			var doc = XDocument.Parse(_fileTypesResource);
			foreach (var f in doc.Descendants("filetype"))
			{
				var name = f.Attributes("name").First().Value;
				var extensions = f.Descendants("extensions").First().Value;
				var extensionSet = new HashSet<string>(extensions.Split(new[]{' ','\n'}).Select(x => "." + x));
				_fileTypesDictionary[name] = extensionSet;
			}
			_fileTypesDictionary[text].UnionWith(_fileTypesDictionary[code]);
			_fileTypesDictionary[text].UnionWith(_fileTypesDictionary[xml]);
			_fileTypesDictionary[searchable] = new HashSet<string>(_fileTypesDictionary[text]);
			_fileTypesDictionary[searchable].UnionWith(_fileTypesDictionary[binary]);
			_fileTypesDictionary[searchable].UnionWith(_fileTypesDictionary[archive]);
		}

		public static FileType FromName(string name)
		{
			string lname = name.ToLowerInvariant();
			if (lname.Equals(archive)) return FileType.Archive;
			if (lname.Equals(binary)) return FileType.Binary;
			if (lname.Equals(text)) return FileType.Text;
			if (lname.Equals(code)) return FileType.Code;
			if (lname.Equals(xml)) return FileType.Xml;
			return FileType.Unknown;
		}

		public FileType GetFileType(FileInfo f)
		{
			if (IsArchiveFile(f)) return FileType.Archive;
			if (IsBinaryFile(f)) return FileType.Binary;
			if (IsTextFile(f)) return FileType.Text;
			if (IsCodeFile(f)) return FileType.Code;
			if (IsXmlFile(f)) return FileType.Xml;
			return FileType.Unknown;
		}

		public bool IsArchiveFile(FileInfo f)
		{
			return _fileTypesDictionary[archive].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsBinaryFile(FileInfo f)
		{
			return _fileTypesDictionary[binary].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsCodeFile(FileInfo f)
		{
			return _fileTypesDictionary[code].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsSearchableFile(FileInfo f)
		{
			return _fileTypesDictionary[searchable].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsTextFile(FileInfo f)
		{
			return _fileTypesDictionary[text].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsUnknownFile(FileInfo f)
		{
			return !IsSearchableFile(f);
		}

		public bool IsXmlFile(FileInfo f)
		{
			return _fileTypesDictionary[xml].Contains(f.Extension.ToLowerInvariant());
		}
	}
}
