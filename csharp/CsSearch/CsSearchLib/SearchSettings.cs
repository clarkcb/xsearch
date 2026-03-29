using System.Collections.Generic;
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
}
