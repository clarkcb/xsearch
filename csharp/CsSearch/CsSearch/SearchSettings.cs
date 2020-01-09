using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace CsSearch
{
	public class SearchSettings
	{
		public bool ArchivesOnly { get; set; }
		public bool Debug { get; set; }
		public bool ExcludeHidden { get; set; }
		public bool FirstMatch { get; set; }
		public ISet<string> InArchiveExtensions { get; private set; }
		public ISet<Regex> InArchiveFilePatterns { get; private set; }
		public ISet<Regex> InDirPatterns { get; private set; }
		public ISet<string> InExtensions { get; private set; }
		public ISet<Regex> InFilePatterns { get; private set; }
		public ISet<FileType> InFileTypes { get; private set; }
		public ISet<Regex> InLinesAfterPatterns { get; private set; }
		public ISet<Regex> InLinesBeforePatterns { get; private set; }
		public int LinesAfter { get; set; }
		public ISet<Regex> LinesAfterToPatterns { get; private set; }
		public ISet<Regex> LinesAfterUntilPatterns { get; private set; }
		public int LinesBefore { get; set; }
		public bool ListDirs { get; set; }
		public bool ListFiles { get; set; }
		public bool ListLines { get; set; }
		public int MaxLineLength { get; set; }
		public bool MultiLineSearch { get; set; }
		public ISet<string> OutArchiveExtensions { get; private set; }
		public ISet<Regex> OutArchiveFilePatterns { get; private set; }
		public ISet<Regex> OutDirPatterns { get; private set; }
		public ISet<string> OutExtensions { get; private set; }
		public ISet<Regex> OutFilePatterns { get; private set; }
		public ISet<FileType> OutFileTypes { get; private set; }
		public ISet<Regex> OutLinesAfterPatterns { get; private set; }
		public ISet<Regex> OutLinesBeforePatterns { get; private set; }
		public bool PrintResults { get; set; }
		public bool PrintUsage { get; set; }
		public bool PrintVersion { get; set; }
		public bool Recursive { get; set; }
		public bool SearchArchives { get; set; }
		public ISet<Regex> SearchPatterns { get; private set; }
		public string StartPath { get; set; }
		public string TextFileEncoding { get; set; }
		public bool UniqueLines { get; set; } = false;
		public bool Verbose { get; set; }

		public SearchSettings()
		{
			ArchivesOnly = false;
			Debug = false;
			ExcludeHidden = true;
			FirstMatch = false;
			InArchiveExtensions = new HashSet<string>();
			InArchiveFilePatterns = new HashSet<Regex>();
			InDirPatterns = new HashSet<Regex>();
			InExtensions = new HashSet<string>();
			InFilePatterns = new HashSet<Regex>();
			InFileTypes = new HashSet<FileType>();
			InLinesAfterPatterns = new HashSet<Regex>();
			InLinesBeforePatterns = new HashSet<Regex>();
			LinesAfter = 0;
			LinesAfterToPatterns = new HashSet<Regex>();
			LinesAfterUntilPatterns = new HashSet<Regex>();
			LinesBefore = 0;
			ListDirs = false;
			ListFiles = false;
			ListLines = false;
			MaxLineLength = 150;
			MultiLineSearch = false;
			OutArchiveExtensions = new HashSet<string>();
			OutArchiveFilePatterns = new HashSet<Regex>();
			OutDirPatterns = new HashSet<Regex>();
			OutExtensions = new HashSet<string>();
			OutFilePatterns = new HashSet<Regex>();
			OutFileTypes = new HashSet<FileType>();
			OutLinesAfterPatterns = new HashSet<Regex>();
			OutLinesBeforePatterns = new HashSet<Regex>();
			PrintResults = false;
			PrintUsage = false;
			PrintVersion = false;
			Recursive = true;
			SearchArchives = false;
			SearchPatterns = new HashSet<Regex>();
			TextFileEncoding = "utf-8";
			UniqueLines = false;
			Verbose = false;
		}

		private static void AddExtension(ISet<string> set, string extList)
		{
			var exts = extList.Split(new[] { ',' });
			foreach (var x in exts)
			{
				var ext = x;
				if (!ext.StartsWith("."))
					ext = "." + ext;
				set.Add(ext.ToLowerInvariant());
			}
		}

		public void AddInExtension(string ext)
		{
			AddExtension(InExtensions, ext);
		}

		public void AddOutExtension(string ext)
		{
			AddExtension(OutExtensions, ext);
		}

		private static void AddPattern(ISet<Regex> set, string pattern)
		{
			set.Add(new Regex(pattern));
		}

		public void AddInDirPattern(string pattern)
		{
			AddPattern(InDirPatterns, pattern);
		}

		public void AddOutDirPattern(string pattern)
		{
			AddPattern(OutDirPatterns, pattern);
		}

		public void AddInFilePattern(string pattern)
		{
			AddPattern(InFilePatterns, pattern);
		}

		public void AddOutFilePattern(string pattern)
		{
			AddPattern(OutFilePatterns, pattern);
		}

		public void AddInArchiveExtension(string ext)
		{
			AddExtension(InArchiveExtensions, ext);
		}

		public void AddOutArchiveExtension(string ext)
		{
			AddExtension(OutArchiveExtensions, ext);
		}

		public void AddInArchiveFilePattern(string pattern)
		{
			AddPattern(InArchiveFilePatterns, pattern);
		}

		public void AddOutArchiveFilePattern(string pattern)
		{
			AddPattern(OutArchiveFilePatterns, pattern);
		}

		public void AddInLinesAfterPattern(string pattern)
		{
			AddPattern(InLinesAfterPatterns, pattern);
		}

		public void AddOutLinesAfterPattern(string pattern)
		{
			AddPattern(OutLinesAfterPatterns, pattern);
		}

		public void AddInLinesBeforePattern(string pattern)
		{
			AddPattern(InLinesBeforePatterns, pattern);
		}

		public void AddOutLinesBeforePattern(string pattern)
		{
			AddPattern(OutLinesBeforePatterns, pattern);
		}

		public void AddLinesAfterToPattern(string pattern)
		{
			AddPattern(LinesAfterToPatterns, pattern);
		}

		public void AddLinesAfterUntilPattern(string pattern)
		{
			AddPattern(LinesAfterUntilPatterns, pattern);
		}

		public void AddSearchPattern(string pattern)
		{
			AddPattern(SearchPatterns, pattern);
		}

		private static void AddFileType(ISet<FileType> set, string typeNameList)
		{
			var typeNames = typeNameList.Split(new[] { ',' });
			foreach (var t in typeNames)
			{
				set.Add(FileTypes.FromName(t));
			}
		}

		public void AddInFileType(string typeName)
		{
			AddFileType(InFileTypes, typeName);
		}

		public void AddOutFileType(string typeName)
		{
			AddFileType(OutFileTypes, typeName);
		}

		public void SetArchivesOnly(bool archivesOnly)
		{
			ArchivesOnly = archivesOnly;
			if (archivesOnly)
				SearchArchives = true;
		}

		public void SetDebug(bool debug)
		{
			Debug = debug;
			if (debug)
				Verbose = true;
		}

		protected static string EnumerableToString<T>(IEnumerable<T> enumerable)
		{
			var sb = new StringBuilder("[");
			var elemCount = 0;
			foreach (var x in enumerable)
			{
				var t = x.GetType();
				if (elemCount > 0)
					sb.Append(", ");
				if (t == typeof(string))
					sb.Append("\"");
				sb.Append(x);
				if (t == typeof(string))
					sb.Append("\"");
				elemCount++;
			}
			sb.Append("]");
			return sb.ToString();
		}

		public override string ToString()
		{
			var sb = new StringBuilder("SearchSettings(");
			sb.Append("ArchivesOnly: " + ArchivesOnly);
			sb.Append(", Debug: " + Debug);
			sb.Append(", ExcludeHidden: " + ExcludeHidden);
			sb.Append(", FirstMatch: " + FirstMatch);
			sb.Append(", InArchiveExtensions: " + EnumerableToString(InArchiveExtensions));
			sb.Append(", InArchiveFilePatterns: " + EnumerableToString(InArchiveFilePatterns));
			sb.Append(", InDirPatterns: " + EnumerableToString(InDirPatterns));
			sb.Append(", InExtensions: " + EnumerableToString(InExtensions));
			sb.Append(", InFilePatterns: " + EnumerableToString(InFilePatterns));
			sb.Append(", InLinesAfterPatterns: " + EnumerableToString(InLinesAfterPatterns));
			sb.Append(", InLinesBeforePatterns: " + EnumerableToString(InLinesBeforePatterns));
			sb.Append(", LinesAfter: " + LinesAfter);
			sb.Append(", LinesAfterToPatterns: " + EnumerableToString(LinesAfterToPatterns));
			sb.Append(", LinesAfterUntilPatterns: " + EnumerableToString(LinesAfterUntilPatterns));
			sb.Append(", LinesBefore: " + LinesBefore);
			sb.Append(", ListDirs: " + ListDirs);
			sb.Append(", ListFiles: " + ListFiles);
			sb.Append(", ListLines: " + ListLines);
			sb.Append(", MaxLineLength: " + MaxLineLength);
			sb.Append(", MultiLineSearch: " + MultiLineSearch);
			sb.Append(", OutArchiveExtensions: " + EnumerableToString(OutArchiveExtensions));
			sb.Append(", OutArchiveFilePatterns: " + EnumerableToString(OutArchiveFilePatterns));
			sb.Append(", OutDirPatterns: " + EnumerableToString(OutDirPatterns));
			sb.Append(", OutExtensions: " + EnumerableToString(OutExtensions));
			sb.Append(", OutFilePatterns: " + EnumerableToString(OutFilePatterns));
			sb.Append(", OutLinesAfterPatterns: " + EnumerableToString(OutLinesAfterPatterns));
			sb.Append(", OutLinesBeforePatterns: " + EnumerableToString(OutLinesBeforePatterns));
			sb.Append(", PrintResults: " + PrintResults);
			sb.Append(", PrintUsage: " + PrintUsage);
			sb.Append(", PrintVersion: " + PrintVersion);
			sb.Append(", Recursive: " + Recursive);
			sb.Append(", SearchArchives: " + SearchArchives);
			sb.Append(", SearchPatterns: " + EnumerableToString(SearchPatterns));
			sb.Append(", StartPath: \"" + StartPath + "\"");
			sb.Append(", TextFileEncoding: \"" + TextFileEncoding + "\"");
			sb.Append(", UniqueLines: " + UniqueLines);
			sb.Append(", Verbose: " + Verbose);
			sb.Append(")");
			return sb.ToString();
		}
	}
}
