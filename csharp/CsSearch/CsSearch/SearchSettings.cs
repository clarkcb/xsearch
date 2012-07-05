using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

namespace CsSearch
{
	public class SearchSettings
	{
		public ISet<string> InExtensions { get; private set; }
		public ISet<string> OutExtensions { get; private set; }
		public ISet<Regex> InDirPatterns { get; private set; }
		public ISet<Regex> OutDirPatterns { get; private set; }
		public ISet<Regex> InFilePatterns { get; private set; }
		public ISet<Regex> OutFilePatterns { get; private set; }
		public ISet<Regex> SearchPatterns { get; private set; }

		private string _startPath;

		public string StartPath
		{
			get { return _startPath; }
			set { _startPath = Path.GetFullPath(value); }
		}

		public bool Debug { get; set; }
		public bool DoTiming { get; set; }
		public bool FirstMatch { get; set; }
		public bool ListFiles { get; set; }
		public bool ListLines { get; set; }
		public bool PrintResults { get; set; }
		public bool PrintUsage { get; set; }
		public bool PrintVersion { get; set; }
		public bool SearchCompressed { get; set; }
		public bool Verbose { get; set; }

		public SearchSettings()
		{
			InExtensions = new HashSet<string>();
			OutExtensions = new HashSet<string>();
			InDirPatterns = new HashSet<Regex>();
			OutDirPatterns = new HashSet<Regex>();
			InFilePatterns = new HashSet<Regex>();
			OutFilePatterns = new HashSet<Regex>();
			SearchPatterns = new HashSet<Regex>();
			DoTiming = false;
			FirstMatch = false;
			ListFiles = false;
			ListLines = false;
			PrintResults = false;
			PrintUsage = false;
			PrintVersion = false;
			SearchCompressed = false;
			Verbose = false;
		}

		public void SetProperty(string name, bool value)
		{
			
		}

		private static void AddExtension(ISet<string> set, string ext)
		{
			if (!ext.StartsWith("."))
				ext = "." + ext;
			set.Add(ext.ToLowerInvariant());
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

		public void AddSearchPattern(string pattern)
		{
			AddPattern(SearchPatterns, pattern);
		}

		private static string EnumerableToString(IEnumerable<object> hashSet)
		{
			var sb = new StringBuilder("[");
			var elemCount = 0;
			foreach (var s in hashSet)
			{
				if (elemCount > 0)
					sb.Append(", ");
				sb.Append("\"" + s + "\"");
				elemCount++;
			}
			sb.Append("]");
			return sb.ToString();
		}

		public override string ToString()
		{
			var sb = new StringBuilder("SearchSettings(");
			sb.Append("StartPath: \"" + StartPath + "\"");
			sb.Append(", InExtensions: " + EnumerableToString(InExtensions));
			sb.Append(", OutExtensions: " + EnumerableToString(OutExtensions));
			sb.Append(", InDirPatterns: " + EnumerableToString(InDirPatterns));
			sb.Append(", OutDirPatterns: " + EnumerableToString(OutDirPatterns));
			sb.Append(", InFilePatterns: " + EnumerableToString(InFilePatterns));
			sb.Append(", OutFilePatterns: " + EnumerableToString(OutFilePatterns));
			sb.Append(", SearchPatterns: " + EnumerableToString(SearchPatterns));
			sb.Append(")");
			return sb.ToString();
		}
	}
}
