using System;
using System.Text;
using CsFindLib;

namespace CsSearchLib;

public class SearchResultFormatter
{
	private SearchSettings Settings { get; }
	public FileResultFormatter FileFormatter { get; }
	private Func<string, string> FormatLineFunc { get; }
	private Func<string, string> FormatMatchFunc { get; }

	public SearchResultFormatter(SearchSettings settings)
	{
		Settings = settings;
		FileFormatter = new FileResultFormatter(settings);
		if (settings.Colorize)
		{
			FormatLineFunc = FormatLineWithColor;
			FormatMatchFunc = FormatMatchWithColor;
		}
		else
		{
			FormatLineFunc = line => line;
			FormatMatchFunc = match => match;
		}
	}

	private string FormatLineWithColor(string line)
	{
		var formattedLine = line;
		foreach (var p in Settings.SearchPatterns)
		{
			var m = p.Match(formattedLine);
			if (m.Success)
			{
				formattedLine = Colorize(formattedLine, m.Index, m.Index + m.Length, Settings.LineColor);
				break;
			}
		}
		return formattedLine;
	}

	public string FormatLine(string line) => FormatLineFunc(line);

	private string FormatMatchWithColor(string match)
	{
		return Colorize(match, 0, match.Length, Settings.LineColor);
	}

	public string FormatMatch(string match) => FormatMatchFunc(match);

	public string Format(SearchResult result)
	{
		if (result.LinesBefore.Count > 0 || result.LinesAfter.Count > 0)
		{
			return MultiLineFormat(result);
		}
		return SingleLineFormat(result);
	}

	private static int LineNumPadding(SearchResult result)
	{
		var maxLineNum = result.LineNum + result.LinesAfter.Count;
		return $"{maxLineNum}".Length;
	}

    private static string Colorize(string s, int matchStartIndex, int matchEndIndex, Color color)
    {
        return FileResultFormatter.Colorize(s, matchStartIndex, matchEndIndex, color);
    }

	private string MultiLineFormat(SearchResult result)
	{
		var filePath = result.File != null ? FileFormatter.FormatFileResult(result.File!) : "<text>";
		var sb = new StringBuilder().
			Append(new string('=', 80)).Append('\n').
			Append(filePath).Append(": ").
			Append(result.LineNum).Append(": ").
			Append('[').Append(result.MatchStartIndex).Append(':').
			Append(result.MatchEndIndex).Append("]\n").
			Append(new string('-', 80)).Append('\n');
		var currentLineNum = result.LineNum;
		var lineFormat = " {0," + LineNumPadding(result) + "} | {1}\n";
		if (result.LinesBefore.Count > 0)
		{
			currentLineNum -= result.LinesBefore.Count;
			foreach (var lineBefore in result.LinesBefore)
			{
				sb.Append(' ').
					Append(string.Format(lineFormat, currentLineNum,
						lineBefore));
				currentLineNum++;
			}
		}

		var line = result.Line;
		if (Settings.Colorize)
		{
			line = Colorize(line!, result.MatchStartIndex - 1, result.MatchEndIndex - 1, Settings.LineColor);
		}
		sb.Append('>').Append(string.Format(lineFormat, result.LineNum, line));
		if (result.LinesAfter.Count > 0)
		{
			currentLineNum++;
			foreach (var lineAfter in result.LinesAfter)
			{
				sb.Append(' ').
					Append(string.Format(lineFormat, currentLineNum,
						lineAfter));
				currentLineNum++;
			}
		}
		return sb.ToString();
	}
        
	private string SingleLineFormat(SearchResult result)
	{
		var filePath = result.File != null ? FileFormatter.FormatFileResult(result.File!) : "<text>";
		var sb = new StringBuilder().Append(filePath);
		if (result.LineNum == 0)
		{
			sb.Append($" matches at [{result.MatchStartIndex}:{result.MatchEndIndex}]");
		}
		else
		{
			sb.Append($": {result.LineNum}: [{result.MatchStartIndex}:{result.MatchEndIndex}]: ");
			sb.Append(FormatResultLine(result));
		}
		return sb.ToString();
	}

	private string FormatResultMatch(SearchResult result)
	{
		if (string.IsNullOrWhiteSpace(result.Line) || Settings.MaxLineLength == 0) return "";

		var matchStartIndex = result.MatchStartIndex - 1;
		var matchEndIndex = result.MatchEndIndex - 1;
		var matchLength = matchEndIndex - matchStartIndex;

		var prefix = "";
		var suffix = "";
		var colorStartIndex = 0;
		var colorEndIndex = matchLength;

		if (matchLength > Settings.MaxLineLength)
		{
			if (matchStartIndex > 2) prefix = "...";
			suffix = "...";
			colorStartIndex = prefix.Length;
			colorEndIndex = Settings.MaxLineLength - 3;
			matchEndIndex = matchStartIndex + colorEndIndex;
			matchStartIndex += colorStartIndex;
		}
		
		var matchString = prefix + result.Line.Substring(matchStartIndex, matchEndIndex - matchStartIndex) + suffix;
		if (Settings.Colorize)
		{
			matchString = Colorize(matchString, colorStartIndex, colorEndIndex, Settings.LineColor);
		}
		return matchString;
	}

	private string FormatResultLine(SearchResult result)
	{
		if (string.IsNullOrWhiteSpace(result.Line) || Settings.MaxLineLength == 0) return "";

		var maxLimit = Settings.MaxLineLength > 0;

		if (maxLimit && result.MatchEndIndex - result.MatchStartIndex > Settings.MaxLineLength)
		{
			return FormatResultMatch(result);
		}

		var lineStartIndex = 0;
		var lineEndIndex = result.Line.Length - 1;
		
		while (char.IsWhiteSpace(result.Line[lineStartIndex]))
		{
			lineStartIndex++;
		}
		while (char.IsWhiteSpace(result.Line[lineEndIndex]))
		{
			lineEndIndex--;
		}

		var matchLength = result.MatchEndIndex - result.MatchStartIndex;
		var matchStartIndex = result.MatchStartIndex - 1 - lineStartIndex;
		var matchEndIndex = matchStartIndex + matchLength;

		var prefix = "";
		var suffix = "";
		
		var trimmedLength = lineEndIndex - lineStartIndex;

		if (maxLimit && trimmedLength > Settings.MaxLineLength)
		{
			lineStartIndex = result.MatchStartIndex - 1;
			lineEndIndex = lineStartIndex + matchLength;
			matchStartIndex = 0;
			matchEndIndex = matchLength;

			var currentLen = lineEndIndex - lineStartIndex;
			while (currentLen < Settings.MaxLineLength)
			{
				if (lineStartIndex > 0)
				{
					lineStartIndex--;
					matchStartIndex++;
					matchEndIndex++;
					currentLen++;
				}

				if (currentLen < Settings.MaxLineLength && lineEndIndex < trimmedLength)
				{
					lineEndIndex++;
					currentLen++;
				}
			}

			if (lineStartIndex > 2)
			{
				prefix = "...";
				lineStartIndex += 3;
			}

			if (lineEndIndex < trimmedLength - 3)
			{
				suffix = "...";
				lineEndIndex -= 3;
			}
		}
		else
		{
			lineEndIndex++;
		}
		
		var formatted = prefix + result.Line.Substring(lineStartIndex, lineEndIndex - lineStartIndex) + suffix;

		if (Settings.Colorize)
		{
			formatted = Colorize(formatted, matchStartIndex, matchEndIndex, Settings.LineColor);
		}
		return formatted;
	}
}
