using System;
using System.IO;

namespace CsSearch
{
	class FileUtil
	{
		public static string GetRelativePath(string fullPath)
		{
			var filePath = fullPath;
			if (filePath.StartsWith(Environment.CurrentDirectory))
				filePath = filePath.Replace(Environment.CurrentDirectory, ".");
			return filePath;
		}

		public static bool IsHiddenFile(FileSystemInfo f)
		{
			return (f.Attributes & FileAttributes.Hidden) != 0;
		}
	}
}
