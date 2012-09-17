using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;

namespace CsSearch
{
	class FileUtil
	{
		private readonly FileInfo _fileTypesPath;
		private readonly IDictionary<string, ISet<string>> _fileTypesDictionary;

		public FileUtil()
		{
			var appSettings = new Properties.Settings();
			_fileTypesPath = new FileInfo(appSettings.FileTypesPath);
			_fileTypesDictionary = new Dictionary<string, ISet<string>>();
			PopulateFileTypes();
		}

		private void PopulateFileTypes()
		{
			//var types = new[] {"binary", "code", "compressed", "nosearch", "text", "unknown", "xml"};
			var doc = XDocument.Load(new StreamReader(_fileTypesPath.FullName));
			foreach (var f in doc.Descendants("filetype"))
			{
				var name = f.Attributes("name").First().Value;
				var extensions = f.Descendants("extensions").First().Value;
				var extensionSet = new HashSet<string>(extensions.Split(' ').Select(x => "." + x));
				_fileTypesDictionary[name] = extensionSet;
			}
			_fileTypesDictionary["text"].UnionWith(_fileTypesDictionary["code"]);
			_fileTypesDictionary["text"].UnionWith(_fileTypesDictionary["xml"]);
			_fileTypesDictionary["searchable"] = new HashSet<string>(_fileTypesDictionary["text"]);
			_fileTypesDictionary["searchable"].UnionWith(_fileTypesDictionary["binary"]);
			_fileTypesDictionary["searchable"].UnionWith(_fileTypesDictionary["compressed"]);
		}

		public bool IsBinaryFile(FileInfo f)
		{
			return _fileTypesDictionary["binary"].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsCompressedFile(FileInfo f)
		{
			return _fileTypesDictionary["compressed"].Contains(f.Extension.ToLowerInvariant());
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
					!_fileTypesDictionary["searchable"].Contains(f.Extension.ToLowerInvariant()));
		}
	}
}
