using System.Collections.Generic;
using System.Text.RegularExpressions;

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
}
