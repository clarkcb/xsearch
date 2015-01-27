using System;
using System.Collections.Generic;
using System.IO;

namespace CsSearch
{
	public class FileUtil
	{
		private static ISet<string> dotDirs = new HashSet<string> { ".", ".." };

		private static char[] dirSeps = new char[] { '/', '\\' };

		public static string GetRelativePath(string fullPath)
		{
			var filePath = fullPath;
			if (filePath.StartsWith(Environment.CurrentDirectory))
				filePath = filePath.Replace(Environment.CurrentDirectory, ".");
			return filePath;
		}

		public static bool IsHiddenFile(FileSystemInfo f)
		{
			var startsWithDot = f.Name.StartsWith(".") && !dotDirs.Contains(f.Name);
			var hasHiddenAttribute = f.Exists && (f.Attributes & FileAttributes.Hidden) != 0;
			return (startsWithDot || hasHiddenAttribute);
		}

		public static string NormalizePath(string path)
		{
			return path.TrimEnd(dirSeps);
		}

		public static string JoinPath(string path1, string path2)
		{
			var dirSep = '/';
			if (path1.IndexOf('\\') > -1)
				dirSep = '\\';
			return NormalizePath(path1) + dirSep + path2;
		}
	}
}
