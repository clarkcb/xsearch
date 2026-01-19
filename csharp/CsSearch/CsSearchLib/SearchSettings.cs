using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using CsFindLib;

namespace CsSearchLib;

public class SearchSettings : FindSettings
{
	private bool _archivesOnly;
	private bool _searchArchives;

	public new bool ArchivesOnly
	{
		get => _archivesOnly;
		set
		{
			_archivesOnly = value;
			if (_archivesOnly) {
				IncludeArchives = true;
				_searchArchives = true;
			}
		}
	}
	public bool FirstMatch { get; set; }
	public ISet<Regex> InLinesAfterPatterns { get; }
	public ISet<Regex> InLinesBeforePatterns { get; }
	public Color LineColor { get; set; }
	public int LinesAfter { get; set; }
	public ISet<Regex> LinesAfterToPatterns { get; }
	public ISet<Regex> LinesAfterUntilPatterns { get; }
	public int LinesBefore { get; set; }
	public int MaxLineLength { get; set; }
	public bool MultiLineSearch { get; set; }
	public ISet<Regex> OutLinesAfterPatterns { get; }
	public ISet<Regex> OutLinesBeforePatterns { get; }
	public bool PrintLines { get; set; }
	public bool PrintMatches { get; set; }
	public bool PrintResults { get; set; }

	public bool SearchArchives
	{
		get => _searchArchives;
		set
		{
			_searchArchives = value;
			if (_searchArchives) {
				IncludeArchives = true;
			}
		}
	}

	public ISet<Regex> SearchPatterns { get; }
	public string TextFileEncoding { get; set; }
	public bool UniqueLines { get; set; }

	public SearchSettings()
	{
		FirstMatch = false;
		InLinesAfterPatterns = new HashSet<Regex>();
		InLinesBeforePatterns = new HashSet<Regex>();
		LineColor = Color.Green;
		LinesAfter = 0;
		LinesAfterToPatterns = new HashSet<Regex>();
		LinesAfterUntilPatterns = new HashSet<Regex>();
		LinesBefore = 0;
		MaxLineLength = 150;
		MultiLineSearch = false;
		OutLinesAfterPatterns = new HashSet<Regex>();
		OutLinesBeforePatterns = new HashSet<Regex>();
        PrintLines = false;
        PrintMatches = false;
		PrintResults = false;
		SearchArchives = false;
		SearchPatterns = new HashSet<Regex>();
		TextFileEncoding = "utf-8";
		UniqueLines = false;
	}

	private static void AddPattern(ISet<Regex> set, string pattern)
	{
		set.Add(new Regex(pattern));
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

	private static string DateTimeToString(System.DateTime? dt)
	{
		if (dt == null)
		{
			return "0";
		}
		return $"\"{dt}\"";
	}

	private static string EnumerableToString<T>(IEnumerable<T> enumerable)
	{
		var sb = new StringBuilder("[");
		var elemCount = 0;
		foreach (var x in enumerable)
		{
			var t = x!.GetType();
			if (elemCount > 0)
				sb.Append(", ");
			if (t == typeof(string))
				sb.Append('"');
			sb.Append(x);
			if (t == typeof(string))
				sb.Append('"');
			elemCount++;
		}
		sb.Append(']');
		return sb.ToString();
	}

	public override string ToString()
	{
		return "SearchSettings(" +
		       "ArchivesOnly: " + ArchivesOnly +
		       ", Colorize: " + Colorize +
		       ", Debug: " + Debug +
		       ", FirstMatch: " + FirstMatch +
		       ", FollowSymlinks: " + FollowSymlinks +
		       ", InArchiveExtensions: " + EnumerableToString(InArchiveExtensions) +
		       ", InArchiveFilePatterns: " + EnumerableToString(InArchiveFilePatterns) +
		       ", IncludeHidden: " + IncludeHidden +
		       ", InDirPatterns: " + EnumerableToString(InDirPatterns) +
		       ", InExtensions: " + EnumerableToString(InExtensions) +
		       ", InFilePatterns: " + EnumerableToString(InFilePatterns) +
		       ", InFileTypes: " + EnumerableToString(InFileTypes) +
		       ", InLinesAfterPatterns: " + EnumerableToString(InLinesAfterPatterns) +
		       ", InLinesBeforePatterns: " + EnumerableToString(InLinesBeforePatterns) +
		       ", LinesAfter: " + LinesAfter +
		       ", LinesAfterToPatterns: " + EnumerableToString(LinesAfterToPatterns) +
		       ", LinesAfterUntilPatterns: " + EnumerableToString(LinesAfterUntilPatterns) +
		       ", LinesBefore: " + LinesBefore +
		       ", MaxDepth: " + MaxDepth +
		       ", MaxLastMod: " + DateTimeToString(MaxLastMod) +
		       ", MaxLineLength: " + MaxLineLength +
		       ", MaxSize: " + MaxSize +
		       ", MinDepth: " + MinDepth +
		       ", MinLastMod: " + DateTimeToString(MinLastMod) +
		       ", MinSize: " + MinSize +
		       ", MultiLineSearch: " + MultiLineSearch +
		       ", OutArchiveExtensions: " + EnumerableToString(OutArchiveExtensions) +
		       ", OutArchiveFilePatterns: " + EnumerableToString(OutArchiveFilePatterns) +
		       ", OutDirPatterns: " + EnumerableToString(OutDirPatterns) +
		       ", OutExtensions: " + EnumerableToString(OutExtensions) +
		       ", OutFilePatterns: " + EnumerableToString(OutFilePatterns) +
		       ", OutFileTypes: " + EnumerableToString(OutFileTypes) +
		       ", OutLinesAfterPatterns: " + EnumerableToString(OutLinesAfterPatterns) +
		       ", OutLinesBeforePatterns: " + EnumerableToString(OutLinesBeforePatterns) +
		       ", Paths: " + EnumerableToString(Paths) +
		       ", PrintDirs: " + PrintDirs +
		       ", PrintFiles: " + PrintFiles +
		       ", PrintLines: " + PrintLines +
		       ", PrintMatches: " + PrintMatches +
		       ", PrintResults: " + PrintResults +
		       ", PrintUsage: " + PrintUsage +
		       ", PrintVersion: " + PrintVersion +
		       ", Recursive: " + Recursive +
		       ", SearchArchives: " + SearchArchives +
		       ", SearchPatterns: " + EnumerableToString(SearchPatterns) +
		       ", SortBy: " + SortByUtil.GetNameFromSortBy(SortBy) +
		       ", SortCaseInsensitive: " + SortCaseInsensitive +
		       ", SortDescending: " + SortDescending +
		       ", TextFileEncoding: \"" + TextFileEncoding + "\"" +
		       ", UniqueLines: " + UniqueLines +
		       ", Verbose: " + Verbose +
		       ")";
	}
}
