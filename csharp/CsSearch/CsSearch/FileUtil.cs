using System;
using System.Collections.Generic;
using System.IO;

namespace CsSearch
{
	public class FileUtil
	{
		private static ISet<string> dotDirs = new HashSet<string> { ".", ".." };

		private static char[] dirSeps = new char[] { '/', '\\' };

		public static IEnumerable<string> EnumerableStringFromFile(SearchFile f)
		{
			return EnumerableStringFromFile(f.FullName);
		}

		public static IEnumerable<string> EnumerableStringFromFile(string filepath)
		{
			using (var sr = new StreamReader(filepath))
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
			if (filepath[0] == '~')
			{
				return JoinPath(GetHomePath(), filepath.Substring(1));
			}
			return filepath;
		}

		public static string GetFileContents(SearchFile f)
		{
			return GetFileContents(f.FullName);
		}

		public static string GetFileContents(string filepath)
		{
			try
			{
				using (var sr = new StreamReader(filepath))
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
			Console.WriteLine("Environment.OSVersion: "+Environment.OSVersion);
			var home = Environment.GetEnvironmentVariable("HOME");
			if (null == home)
				return Environment.GetEnvironmentVariable("USERPROFILE");
			return home;
		}

		public static string GetRelativePath(string fullPath, string startpath)
		{
			var filePath = fullPath;
			if (startpath == ".")
			{
				filePath = filePath.Replace(Environment.CurrentDirectory, ".");
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
			return dotDirs.Contains(NormalizePath(filename));
		}

		public static bool IsHidden(FileSystemInfo f)
		{
			var startsWithDot = f.Name.StartsWith(".") && !IsDotDir(f.Name);
			var hasHiddenAttribute = f.Exists && (f.Attributes & FileAttributes.Hidden) != 0;
			return (startsWithDot || hasHiddenAttribute);
		}

		public static string JoinPath(string path1, string path2)
		{
			var dirSep = '/';
			if (path1.IndexOf('\\') > -1)
				dirSep = '\\';
			if (path2[0] == '/' || path2[0] == '\\')
				path2 = path2.Substring(1);
			return NormalizePath(path1) + dirSep + path2;
		}

		public static string NormalizePath(string path)
		{
			return path.TrimEnd(dirSeps);
		}
	}
}
