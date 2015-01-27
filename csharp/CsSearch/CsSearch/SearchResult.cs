using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace CsSearch
{
	public class SearchResult
	{
		public Regex SearchPattern { get; private set; }
		public SearchFile File { get; set; }
		public int LineNum { get; private set; }
		public int MatchStartIndex { get; private set; }
		public int MatchEndIndex { get; private set; }
		public string Line { get; private set; }
		public IList<string> LinesBefore { get; private set; }
		public IList<string> LinesAfter { get; private set; }

		// temp
		private const int MaxLineLength = 150;

		public SearchResult(Regex searchPattern, SearchFile file, int lineNum,
			int matchStartIndex, int matchEndIndex, string line)
		{
			Initialize(searchPattern, file, lineNum, matchStartIndex,
				matchEndIndex, line, new List<string>(), new List<string>());
		}

		public SearchResult(Regex searchPattern, SearchFile file, int lineNum,
			int matchStartIndex, int matchEndIndex, string line,
			IList<string> linesBefore, IList<string> linesAfter)
		{
			Initialize(searchPattern, file, lineNum, matchStartIndex,
				matchEndIndex, line, linesBefore, linesAfter);

		}

		private void Initialize(Regex searchPattern, SearchFile file,
			int lineNum, int matchStartIndex, int matchEndIndex, string line,
			IList<string> linesBefore, IList<string> linesAfter)
		{
			SearchPattern = searchPattern;
			File = file;
			LineNum = lineNum;
			MatchStartIndex = matchStartIndex;
			MatchEndIndex = matchEndIndex;
			Line = line;
			LinesBefore = linesBefore;
			LinesAfter = linesAfter;
		}

		public override string ToString()
		{
			if (LinesBefore.Count > 0 || LinesAfter.Count > 0)
			{
				return MultiLineToString();
			}
			return SingleLineToString();
		}

		private int LineNumPadding()
		{
			int maxLineNum = LineNum + LinesAfter.Count;
			return string.Format("{0}", maxLineNum).Length;
		}

		private string MultiLineToString()
		{
			var sb = new StringBuilder().
				Append(new String('=', 80)).Append('\n').
				Append(FileUtil.GetRelativePath(File.FullName)).Append(": ").
				Append(LineNum).Append(": ").
				Append('[').Append(MatchStartIndex).Append(':').
				Append(MatchEndIndex).Append("]\n").
				Append(new String('-', 80)).Append('\n');
			int currentLineNum = LineNum;
			string lineFormat = " {0,-" + LineNumPadding() + "} | {1}\n";
			if (LinesBefore.Count > 0)
			{
				currentLineNum -= LinesBefore.Count;
				foreach (string lineBefore in LinesBefore)
				{
					sb.Append(' ').
						Append(string.Format(lineFormat, currentLineNum, lineBefore));
					currentLineNum++;
				}
			}
			sb.Append('>').Append(string.Format(lineFormat, LineNum, Line));
			if (LinesAfter.Count > 0)
			{
				currentLineNum++;
				foreach (string lineAfter in LinesAfter)
				{
					sb.Append(' ').
						Append(string.Format(lineFormat, currentLineNum, lineAfter));
					currentLineNum++;
				}
			}
			return sb.ToString();
		}

		private string SingleLineToString()
		{
			var sb = new StringBuilder().Append(FileUtil.GetRelativePath(File.FullName));
			if (LineNum == 0)
			{
				//sb.Append(" has match for pattern \"" + SearchPattern + "\"");
				sb.Append(" matches");
			}
			else
			{
				sb.Append(string.Format(": {0}: [{1}:{2}]: ", LineNum,
					MatchStartIndex, MatchEndIndex));
				sb.Append(FormatMatchingLine());
			}
			return sb.ToString();
		}

		private string FormatMatchingLine()
		{
			var formatted = Line;
			var lineLength = Line.Length;
			var matchLength = MatchEndIndex - MatchStartIndex;

			if (lineLength > MaxLineLength)
			{
				var adjustedMaxLength = MaxLineLength - matchLength;
				var beforeIndex = MatchStartIndex;
				if (MatchStartIndex > 0)
				{
					beforeIndex = beforeIndex - (adjustedMaxLength / 4);
					if (beforeIndex < 0)
						beforeIndex = 0;
				}
				adjustedMaxLength = adjustedMaxLength - (MatchStartIndex - beforeIndex);
				var afterIndex = MatchEndIndex + adjustedMaxLength;
				if (afterIndex > lineLength)
					afterIndex = lineLength;

				var before = "";
				if (beforeIndex > 3)
				{
					before = "...";
					beforeIndex += 3;
				}
				var after = "";
				if (afterIndex < lineLength - 3)
				{
					after = "...";
					afterIndex -= 3;
				}
				formatted = before + Line.Substring(beforeIndex, afterIndex-beforeIndex) + after;
			}
			return formatted.Trim();
		}
	}
}
