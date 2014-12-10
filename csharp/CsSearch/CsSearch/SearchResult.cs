using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

namespace CsSearch
{
	public class SearchResult
	{
		public Regex SearchPattern { get; private set; }
		public FileInfo File { get; private set; }
		public int LineNum { get; private set; }
		public int MatchStartIndex { get; private set; }
		public int MatchEndIndex { get; private set; }
		public string Line { get; private set; }

		// temp
		private const int MAXLINELENGTH = 150;

		public SearchResult(Regex searchPattern, FileInfo file, int lineNum,
			int matchStartIndex, int matchEndIndex, string line)
		{
			SearchPattern = searchPattern;
			File = file;
			LineNum = lineNum;
			MatchStartIndex = matchStartIndex;
			MatchEndIndex = matchEndIndex;
			Line = line;
		}

		public override string ToString()
		{
			var filePath = File.FullName;
			if (filePath.StartsWith(Environment.CurrentDirectory))
				filePath = filePath.Replace(Environment.CurrentDirectory, ".");
			var sb = new StringBuilder().Append(filePath);
			if (LineNum == 0)
			{
				//sb.Append(" has match for pattern \"" + SearchPattern + "\"");
				sb.Append(" matches");
			}
			else
			{
				sb.Append(string.Format(": {0} [{1}:{2}]: ", LineNum,
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

			if (lineLength > MAXLINELENGTH)
			{
				var adjustedMaxLength = MAXLINELENGTH - matchLength;
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
