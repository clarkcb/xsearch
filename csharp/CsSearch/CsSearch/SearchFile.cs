using System.Collections.Generic;
using System.IO;
using System.Text;

namespace CsSearch
{
	public class SearchFile
	{
		public static string ContainerSeparator = "!";

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
			get
			{
				return FileUtil.JoinPath(FilePath, FileName);
			}
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

		public void AddContainer(string container)
		{
			Containers.Add(container);
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
				for (var i = 0; i < Containers.Count; i++)
				{
					if (i > 0) sb.Append(ContainerSeparator);
					sb.Append(Containers[i]);
				}
				sb.Append(ContainerSeparator);
			}
			sb.Append(PathAndName);
			return sb.ToString();
		}
	}
}
