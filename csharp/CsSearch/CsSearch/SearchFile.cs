using System.Collections.Generic;
using System.IO;
using System.Text;

namespace CsSearch
{
	public class SearchFile
	{
		public static string CONTAINER_SEPARATOR = "!";

		public IList<string> Containers { get; private set; }
		public string FilePath { get; private set; }
		public string FileName { get; private set; }
		public FileType Type { get; private set; }

		public string FullName
		{
			get { return ToString(); }
		}

		public string PathAndName
		{
			get { return FilePath + Path.DirectorySeparatorChar + FileName; }
		}

		public SearchFile(string path, string fileName, FileType type) : this(new List<string>(), path, fileName, type) {}

		public SearchFile(IList<string> containers, string path,
			string fileName, FileType type)
		{
			Containers = containers;
			FilePath = path;
			FileName = fileName;
			Type = type;
		}

		public FileInfo ToFileInfo()
		{
			return new FileInfo(PathAndName);
		}

		public override string ToString()
		{
			var sb = new StringBuilder();
			if (Containers.Count > 0)
			{
				for (int i = 0; i < Containers.Count; i++)
				{
					if (i > 0) sb.Append(CONTAINER_SEPARATOR);
					sb.Append(Containers[i]);
				}
				sb.Append(CONTAINER_SEPARATOR);
			}
			if (FilePath != null && !FilePath.Equals(""))
			{
				sb.Append(FilePath).Append(Path.DirectorySeparatorChar);
			}
			sb.Append(FileName);
			return sb.ToString();
		}
	}
}
