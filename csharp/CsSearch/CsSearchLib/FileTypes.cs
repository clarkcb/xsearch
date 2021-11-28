using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Xml.Linq;

using FileTypesDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,object>>>;

namespace CsSearchLib
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

		private const string Archive = "archive";
		private const string Binary = "binary";
		private const string Code = "code";
		private const string Searchable = "searchable";
		private const string Text = "text";
		private const string Xml = "xml";

		public FileTypes()
		{
			// _fileTypesResource = EmbeddedResource.GetResourceFileContents("CsSearch.Resources.filetypes.xml");
			_fileTypesResource = EmbeddedResource.GetResourceFileContents("CsSearch.Resources.filetypes.json");
			_fileTypesDictionary = new Dictionary<string, ISet<string>>();
			// PopulateFileTypesFromXml();
			PopulateFileTypesFromJson();
		}

		private void PopulateFileTypesFromJson()
		{
			var filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(_fileTypesResource);
			var filetypeDicts = filetypesDict["filetypes"];
			foreach (var filetypeDict in filetypeDicts)
			{
				var name = ((JsonElement)filetypeDict["type"]).GetString();
				var extensions = ((JsonElement)filetypeDict["extensions"]).EnumerateArray()
					.Select(x => "." + x.GetString());
				var extensionSet = new HashSet<string>(extensions);
				_fileTypesDictionary[name] = extensionSet;
			}
			_fileTypesDictionary[Text].UnionWith(_fileTypesDictionary[Code]);
			_fileTypesDictionary[Text].UnionWith(_fileTypesDictionary[Xml]);
			_fileTypesDictionary[Searchable] = new HashSet<string>(_fileTypesDictionary[Text]);
			_fileTypesDictionary[Searchable].UnionWith(_fileTypesDictionary[Binary]);
			_fileTypesDictionary[Searchable].UnionWith(_fileTypesDictionary[Archive]);
		}

		private void PopulateFileTypesFromXml()
		{
			var doc = XDocument.Parse(_fileTypesResource);
			foreach (var f in doc.Descendants("filetype"))
			{
				var name = f.Attributes("name").First().Value;
				var extensions = f.Descendants("extensions").First().Value;
				var extensionSet = new HashSet<string>(extensions.Split(new[]{' ', '\n'}).Select(x => "." + x));
				_fileTypesDictionary[name] = extensionSet;
			}
			_fileTypesDictionary[Text].UnionWith(_fileTypesDictionary[Code]);
			_fileTypesDictionary[Text].UnionWith(_fileTypesDictionary[Xml]);
			_fileTypesDictionary[Searchable] = new HashSet<string>(_fileTypesDictionary[Text]);
			_fileTypesDictionary[Searchable].UnionWith(_fileTypesDictionary[Binary]);
			_fileTypesDictionary[Searchable].UnionWith(_fileTypesDictionary[Archive]);
		}

		public static FileType FromName(string name)
		{
			return string.IsNullOrEmpty(name)
				? FileType.Unknown
				: name.ToLowerInvariant() switch
			{
				Archive => FileType.Archive,
				Binary => FileType.Binary,
				Code => FileType.Code,
				Text => FileType.Text,
				Xml => FileType.Xml,
				_ => FileType.Unknown
			};
		}

		public FileType GetFileType(FileInfo f)
		{
			if (IsArchiveFile(f)) return FileType.Archive;
			if (IsBinaryFile(f)) return FileType.Binary;
			if (IsCodeFile(f)) return FileType.Code;
			if (IsXmlFile(f)) return FileType.Xml;
			return IsTextFile(f) ? FileType.Text : FileType.Unknown;
		}

		public bool IsArchiveFile(FileInfo f)
		{
			return _fileTypesDictionary[Archive].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsBinaryFile(FileInfo f)
		{
			return _fileTypesDictionary[Binary].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsCodeFile(FileInfo f)
		{
			return _fileTypesDictionary[Code].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsSearchableFile(FileInfo f)
		{
			return _fileTypesDictionary[Searchable].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsTextFile(FileInfo f)
		{
			return _fileTypesDictionary[Text].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsUnknownFile(FileInfo f)
		{
			return !IsSearchableFile(f);
		}

		public bool IsXmlFile(FileInfo f)
		{
			return _fileTypesDictionary[Xml].Contains(f.Extension.ToLowerInvariant());
		}
	}
}
