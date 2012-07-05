using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CsSearch
{
	class FileUtil
	{
		public ISet<string> BinaryExtensions { get; private set; }
		public ISet<string> CompressedExtensions { get; private set; }
		public ISet<string> NosearchExtensions { get; private set; }
		public ISet<string> SearchableExtensions { get; private set; }
		public ISet<string> TextExtensions { get; private set; }
		public ISet<string> UnknownExtensions { get; private set; }

		public FileUtil()
		{
			var appSettings = new Properties.Settings();
			BinaryExtensions = new HashSet<string>(appSettings.BinaryExtensions.Split(' ').Select(x => "." + x));
			CompressedExtensions = new HashSet<string>(appSettings.CompressedExtensions.Split(' ').Select(x => "." + x));
			NosearchExtensions = new HashSet<string>(appSettings.NoSearchExtensions.Split(' ').Select(x => "." + x));
			TextExtensions = new HashSet<string>(appSettings.TextExtensions.Split(' ').Select(x => "." + x));
			UnknownExtensions = new HashSet<string>(appSettings.UnknownExtensions.Split(' ').Select(x => "." + x));
			SearchableExtensions = new HashSet<string>(BinaryExtensions);
			SearchableExtensions.UnionWith(CompressedExtensions);
			SearchableExtensions.UnionWith(TextExtensions);
		}

		public bool IsBinaryFile(FileInfo f)
		{
			return BinaryExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsCompressedFile(FileInfo f)
		{
			return CompressedExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsSearchableFile(FileInfo f)
		{
			return SearchableExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsTextFile(FileInfo f)
		{
			return TextExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsUnknownFile(FileInfo f)
		{
			return (UnknownExtensions.Contains(f.Extension.ToLowerInvariant()) ||
					(!SearchableExtensions.Contains(f.Extension.ToLowerInvariant()) &&
					 !UnknownExtensions.Contains(f.Extension.ToLowerInvariant())));
		}

	}
}
