using System;
using System.Text;
using CsFindLib;

namespace CsSearchLib;

public class SearchResultFormatter
{
	private SearchSettings Settings { get; }
	public FileResultFormatter FileFormatter { get; }
	private Func<string, string> FormatLineFunc { get; }

	public SearchResultFormatter(SearchSettings settings)
	{
		Settings = settings;
		FileFormatter = new FileResultFormatter(settings);
		if (settings.Colorize)
		{
			FormatLineFunc = FormatLineWithColor;
		}
		else
		{
			FormatLineFunc = line => line;
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
			sb.Append(FormatMatchingLine(result));
		}
		return sb.ToString();
	}

	private string FormatMatchingLine(SearchResult result)
	{
		var formatted = result.Line!.TrimEnd();
		var leadingWhitespaceCount = 0;
		while (char.IsWhiteSpace(formatted[leadingWhitespaceCount]))
		{
			leadingWhitespaceCount++;
		}
		formatted = formatted.Trim();
		var formattedLength = formatted.Length;
		var maxLineEndIndex = formattedLength - 1;
		var matchLength = result.MatchEndIndex - result.MatchStartIndex;
		var matchStartIndex = result.MatchStartIndex - 1 - leadingWhitespaceCount;
		var matchEndIndex = matchStartIndex + matchLength;

		if (formattedLength > Settings.MaxLineLength)
		{
			var lineStartIndex = matchStartIndex;
			var lineEndIndex = lineStartIndex + matchLength;
			matchStartIndex = 0;
			matchEndIndex = matchLength;

			while (lineEndIndex > formattedLength - 1)
			{
				lineStartIndex--;
				lineEndIndex--;
				matchStartIndex++;
				matchEndIndex++;
			}

			formattedLength = lineEndIndex - lineStartIndex;
			while (formattedLength < Settings.MaxLineLength)
			{
				if (lineStartIndex > 0)
				{
					lineStartIndex--;
					matchStartIndex++;
					matchEndIndex++;
					formattedLength = lineEndIndex - lineStartIndex;
				}
				if (formattedLength < Settings.MaxLineLength && lineEndIndex < maxLineEndIndex)
				{
					lineEndIndex++;
				}
				formattedLength = lineEndIndex - lineStartIndex;
			}

			formatted = formatted.Substring(lineStartIndex, formattedLength);

			if (lineStartIndex > 2)
			{
				formatted = "..." + formatted.Substring(3);
			}

			if (lineEndIndex < maxLineEndIndex - 3)
			{
				formatted = formatted.Substring(0, formattedLength - 3) + "...";
			}
		}

		if (Settings.Colorize)
		{
			formatted = Colorize(formatted, matchStartIndex, matchEndIndex, Settings.LineColor);
		}
		return formatted;
	}
}
