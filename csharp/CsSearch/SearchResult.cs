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
		public string Line { get; private set; }

		public SearchResult(Regex searchPattern, FileInfo file, int lineNum, string line)
		{
			SearchPattern = searchPattern;
			File = file;
			LineNum = lineNum;
			Line = line;
		}

		public override string ToString()
		{
			var sb = new StringBuilder().
				Append(File.FullName);
			if (LineNum == 0)
			{
				sb.Append(" has match for pattern \"" + SearchPattern + "\"");
			}
			else
			{
				sb.Append(": ");
				sb.Append(LineNum + ": ");
				sb.Append(Line.Trim());
			}
			return sb.ToString();
		}
	}
}
