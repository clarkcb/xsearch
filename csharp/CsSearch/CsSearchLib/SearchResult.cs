using System.Collections.Generic;
using System.Text.RegularExpressions;
using System;

using CsFindLib;

namespace CsSearchLib;

public class SearchResult(
	Regex searchPattern,
	FileResult? file,
	int lineNum,
	int matchStartIndex,
	int matchEndIndex,
	string? line,
	IList<string> linesBefore,
	IList<string> linesAfter)
{
	public Regex SearchPattern { get; private set; } = searchPattern;
	public FileResult? File { get; set; } = file;
	public int LineNum { get; } = lineNum;
	public int MatchStartIndex { get; } = matchStartIndex;
	public int MatchEndIndex { get; private set; } = matchEndIndex;
	public string? Line { get; private set; } = line;
	public IList<string> LinesBefore { get; private set; } = linesBefore;
	public IList<string> LinesAfter { get; private set; } = linesAfter;

	public SearchResult(Regex searchPattern, FileResult? file, int lineNum,
		int matchStartIndex, int matchEndIndex, string? line)
		: this(searchPattern, file, lineNum, matchStartIndex, matchEndIndex, line,
			new List<string>(), new List<string>())
	{
	}

	private int CompareByMatchLocation(SearchResult other)
	{
		if (LineNum != other.LineNum) 
			return LineNum -  other.LineNum;
		return MatchStartIndex - other.MatchStartIndex;
	}

	public int CompareByPath(SearchResult other, bool caseInsensitive)
	{
		if (File != null && other.File != null)
		{
			var cmp = File.CompareByPath(other.File, caseInsensitive);
			if (cmp != 0)
				return cmp;
		}

		return CompareByMatchLocation(other);
	}

	public int CompareByName(SearchResult other, bool caseInsensitive)
	{
		if (File != null && other.File != null)
		{
			var cmp = File.CompareByName(other.File, caseInsensitive);
			if (cmp != 0)
				return cmp;
		}

		return CompareByMatchLocation(other);
	}

	public int CompareBySize(SearchResult other, bool caseInsensitive)
	{
		if (File != null && other.File != null)
		{
			var cmp = File.CompareBySize(other.File, caseInsensitive);
			if (cmp != 0)
				return cmp;
		}

		return CompareByMatchLocation(other);
	}

	public int CompareByType(SearchResult other, bool caseInsensitive)
	{
		if (File != null && other.File != null)
		{
			var cmp = File.CompareByType(other.File, caseInsensitive);
			if (cmp != 0)
				return cmp;
		}

		return CompareByMatchLocation(other);
	}

	public int CompareByLastMod(SearchResult other, bool caseInsensitive)
	{
		if (File != null && other.File != null)
		{
			var cmp = File.CompareByLastMod(other.File, caseInsensitive);
			if (cmp != 0)
				return cmp;
		}

		return CompareByMatchLocation(other);
	}
}
