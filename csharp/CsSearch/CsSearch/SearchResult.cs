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

		public SearchResult(Regex searchPattern, SearchFile file, int lineNum,
			int matchStartIndex, int matchEndIndex, string line)
		{
			Initialize(searchPattern, file, lineNum, matchStartIndex,
				matchEndIndex, line, new List<string>(), new List<string>());
		}

		public SearchResult(Regex searchPattern, SearchFile file, int lineNum, int matchStartIndex,
			int matchEndIndex, string line, IList<string> linesBefore, IList<string> linesAfter)
		{
			Initialize(searchPattern, file, lineNum, matchStartIndex,
				matchEndIndex, line, linesBefore, linesAfter);
		}

		private void Initialize(Regex searchPattern, SearchFile file, int lineNum, int matchStartIndex,
			int matchEndIndex, string line, IList<string> linesBefore, IList<string> linesAfter)
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

		public static int Compare(SearchResult r1, SearchResult r2)
		{
			if (r1 == null && r2 == null)
				return 0;
			if (r1 == null)
				return -1;
			if (r2 == null)
				return 1;
			if (r1.File != null && r2.File != null)
			{
				var sfCmp = SearchFile.Compare(r1.File, r2.File);
				if (sfCmp != 0)
				{
					return sfCmp;
				}
			}
			if (r1.LineNum == r2.LineNum)
			{
				return r1.MatchStartIndex - r2.MatchStartIndex;
			}
			return r1.LineNum - r2.LineNum;
		}

	}

	public class SearchResultsComparer : IComparer<SearchResult>
	{
		public int Compare(SearchResult r1, SearchResult r2)
		{
			return SearchResult.Compare(r1, r2);
		}
	}
}
