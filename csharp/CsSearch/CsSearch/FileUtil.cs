using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;

namespace CsSearch
{
	public enum FileType
	{
		Archive,
		Binary,
		Text,
		Unknown
	};

	class FileUtil
	{
		public readonly ISet<string> CurrentAndParentDirs = new HashSet<string> {".", ".."};

		private readonly string _fileTypesResource;
		private readonly IDictionary<string, ISet<string>> _fileTypesDictionary;

		public FileUtil()
		{
			_fileTypesResource = Properties.Resources.filetypes;
			_fileTypesDictionary = new Dictionary<string, ISet<string>>();
			PopulateFileTypes();
		}

		private void PopulateFileTypes()
		{
			var doc = XDocument.Parse(_fileTypesResource);
			foreach (var f in doc.Descendants("filetype"))
			{
				var name = f.Attributes("name").First().Value;
				//Console.WriteLine("name: " + name);
				var extensions = f.Descendants("extensions").First().Value;
				//Console.WriteLine("extensions: " + extensions);
				var extensionSet = new HashSet<string>(extensions.Split(new char[]{' ','\n'}).Select(x => "." + x));
				_fileTypesDictionary[name] = extensionSet;
			}
			_fileTypesDictionary["text"].UnionWith(_fileTypesDictionary["code"]);
			_fileTypesDictionary["text"].UnionWith(_fileTypesDictionary["xml"]);
			_fileTypesDictionary["searchable"] = new HashSet<string>(_fileTypesDictionary["text"]);
			_fileTypesDictionary["searchable"].UnionWith(_fileTypesDictionary["binary"]);
			_fileTypesDictionary["searchable"].UnionWith(_fileTypesDictionary["archive"]);
		}

		public FileType GetFileType(FileInfo f)
		{
			if (IsArchiveFile(f)) return FileType.Archive;
			if (IsBinaryFile(f)) return FileType.Binary;
			if (IsTextFile(f)) return FileType.Text;
			return FileType.Unknown;
		}

		public bool IsBinaryFile(FileInfo f)
		{
			return _fileTypesDictionary["binary"].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsArchiveFile(FileInfo f)
		{
			return _fileTypesDictionary["archive"].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsSearchableFile(FileInfo f)
		{
			return _fileTypesDictionary["searchable"].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsTextFile(FileInfo f)
		{
			return _fileTypesDictionary["text"].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsUnknownFile(FileInfo f)
		{
			return (_fileTypesDictionary["unknown"].Contains(f.Extension.ToLowerInvariant()) ||
					(!_fileTypesDictionary["searchable"].Contains(f.Extension.ToLowerInvariant()) &&
					(!_fileTypesDictionary["nosearch"].Contains(f.Extension.ToLowerInvariant()))));
		}
	}
}
