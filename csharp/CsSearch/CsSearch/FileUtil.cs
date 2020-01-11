using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace CsSearch
{
	public class FileUtil
	{
		private const string CurrentPath = ".";
		private const string ParentPath = "..";
		private static readonly ISet<string> DotDirs = new HashSet<string> { CurrentPath, ParentPath };

		private const char ForwardSlash = '/';
		private const char BackSlash = '\\';
		private static readonly char[] DirSeps = new char[] { ForwardSlash, BackSlash };

		public static IEnumerable<string> EnumerableStringFromFile(SearchFile f, Encoding enc)
		{
			return EnumerableStringFromFile(f.FullName, enc);
		}

		public static IEnumerable<string> EnumerableStringFromFile(string filepath, Encoding enc)
		{
			using (var sr = new StreamReader(filepath, enc))
			{
				// read each line, ensuring not null (EOF)
				string line;
				while ((line = sr.ReadLine()) != null)
				{
					// return trimmed line
					yield return line;
				}
			}
		}

		public static string ExpandPath(string filepath)
		{
			return filepath[0] == '~' ? JoinPath(GetHomePath(), filepath.Substring(1)) : filepath;
		}

		public static string ContractPath(string filepath)
		{
			return filepath[0] == '~' ? filepath : filepath.Replace(GetHomePath(), "~");
		}

		public static string GetFileContents(SearchFile f, Encoding encoding)
		{
			return GetFileContents(f.FullName, encoding);
		}

		public static string GetFileContents(string filepath, Encoding encoding)
		{
			try
			{
				using (var sr = new StreamReader(filepath, encoding))
				{
					var contents = sr.ReadToEnd();
					return contents;
				}
			}
			catch (IOException e)
			{
				throw e;
			}
		}

		public static string GetHomePath()
		{
			var home = Environment.GetEnvironmentVariable("HOME");
			if (null == home)
				return Environment.GetEnvironmentVariable("USERPROFILE");
			return home;
		}

		public static string GetRelativePath(string fullPath, string startpath)
		{
			var filePath = fullPath;
			if (IsDotDir(startpath))
			{
				if (NormalizePath(startpath) == CurrentPath)
				{
					filePath = filePath.Replace(Environment.CurrentDirectory, ".");
				}
				else if (NormalizePath(startpath) == ParentPath)
				{
					var parentDirectory = NormalizePath(new DirectoryInfo(startpath).FullName);
					filePath = filePath.Replace(parentDirectory, "..");
				}
			}
			return filePath;
		}

		public static bool IsDirectory(string filename)
		{
			try
			{
				var attr = File.GetAttributes(filename);
				return ((attr & FileAttributes.Directory) == FileAttributes.Directory);
			}
			catch (DirectoryNotFoundException)
			{
				return false;
			}
			catch (FileNotFoundException)
			{
				return false;
			}
		}

		public static bool IsDotDir(string filename)
		{
			return DotDirs.Contains(NormalizePath(filename));
		}

		public static bool IsHidden(FileSystemInfo f)
		{
			var startsWithDot = f.Name.StartsWith(CurrentPath) && !IsDotDir(f.Name);
			var hasHiddenAttribute = f.Exists && (f.Attributes & FileAttributes.Hidden) != 0;
			return (startsWithDot || hasHiddenAttribute);
		}

		public static string JoinPath(string path1, string path2)
		{
			var dirSep = ForwardSlash;
			if (path1.IndexOf(BackSlash) > -1)
				dirSep = BackSlash;
			if (path2[0] == ForwardSlash || path2[0] == BackSlash)
				path2 = path2.Substring(1);
			return NormalizePath(path1) + dirSep + path2;
		}

		public static string NormalizePath(string path)
		{
			return path.TrimEnd(DirSeps);
		}
	}
}
