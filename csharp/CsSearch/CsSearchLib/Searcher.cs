using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using CsFindLib;

namespace CsSearchLib;

public class Searcher
{
	private readonly Finder _finder;
	private SearchSettings Settings { get; }
	private ConcurrentBag<SearchResult> Results { get; }
	private Encoding TextFileEncoding { get; set; } = Encoding.Default;

	// TODO: move this to SearchSettings
	private const int FileBatchSize = 255;
	// use single-byte encoding for reading binary files (will break if UTF8)
	private readonly Encoding _binaryEncoding = Encoding.GetEncoding("ISO-8859-1");

	public Searcher(SearchSettings settings)
	{
		Settings = settings;
		try
		{
			_finder = new Finder(settings);
		}
		catch (FindException e)
		{
			throw new SearchException(e.Message);
		}
		ValidateSettings();
		Results = [];
	}

	private void ValidateSettings()
	{
		if (Settings.SearchPatterns.Count < 1)
			throw new SearchException("No search patterns defined");
		try
		{
			TextFileEncoding = Encoding.GetEncoding(Settings.TextFileEncoding);
		}
		catch (ArgumentException)
		{
			throw new SearchException("Invalid encoding");
		}
		if (Settings.LinesBefore < 0)
			throw new SearchException("Invalid linesbefore");
		if (Settings.LinesAfter < 0)
			throw new SearchException("Invalid linesafter");
		if (Settings.MaxLineLength < 0)
			throw new SearchException("Invalid maxlinelength");
	}

	private void AddSearchResult(SearchResult searchResult)
	{
		Results.Add(searchResult);
	}

	private static bool LinesMatch(IEnumerable<string> lines,
		ICollection<Regex> inPatterns, ICollection<Regex> outPatterns)
	{
		var lineList = lines.ToList();
		return ((inPatterns.Count == 0 || AnyMatchesAnyPattern(lineList, inPatterns))
		        && (outPatterns.Count == 0 || !AnyMatchesAnyPattern(lineList, outPatterns)));
	}

	private bool LinesBeforeMatch(IEnumerable<string> linesBefore)
	{
		return LinesMatch(linesBefore, Settings.InLinesBeforePatterns,
			Settings.OutLinesBeforePatterns);
	}

	private bool LinesAfterMatch(IEnumerable<string> linesAfter)
	{
		return LinesMatch(linesAfter, Settings.InLinesAfterPatterns,
			Settings.OutLinesAfterPatterns);
	}

	private static IEnumerable<int> GetNewLineIndices(string text)
	{
		// to keep it simple, only get indices of '\n' and deal with '\r' later
		IList<int> newLineIndices = new List<int>();
		for (var i = 0; i < text.Length; i++)
		{
			if (text[i] == '\n')
				newLineIndices.Add(i);
		}
		return newLineIndices;
	}

	private static IEnumerable<int> GetStartLineIndices(IEnumerable<int> newLineIndices)
	{
		var startLineIndices = new List<int> {0};
		startLineIndices.AddRange(newLineIndices.Select(i => i + 1));
		return startLineIndices;
	}

	private static IEnumerable<int> GetEndLineIndices(string text,
		IEnumerable<int> newLineIndices)
	{
		var endLineIndices = new List<int>(newLineIndices) {text.Length - 1};
		return endLineIndices;
	}

	private IEnumerable<string> GetLinesBeforeFromContents(string content,
		IEnumerable<int> beforeStartIndices, IEnumerable<int> beforeEndIndices)
	{
		var linesBefore = new List<string>();
		if (Settings.LinesBefore == 0) return linesBefore;
		var starts = beforeStartIndices.Reverse().Take(Settings.LinesBefore).Reverse().ToList();
		var ends = beforeEndIndices.Reverse().Take(Settings.LinesBefore + 1).Reverse().Skip(1).ToList();
		linesBefore.AddRange(starts.Select(
			(t, i) => content.Substring(t, ends[i] - t)));
		return linesBefore;
	}

	private IEnumerable<string> GetLinesAfterFromContents(string content,
		IEnumerable<int> afterStartIndices, IEnumerable<int> afterEndIndices)
	{
		var linesAfter = new List<string>();
		if (Settings.LinesAfter == 0) return linesAfter;
		var starts = afterStartIndices.Take(Settings.LinesAfter).ToList();
		var ends = afterEndIndices.Skip(1).Take(Settings.LinesAfter).ToList();
		linesAfter.AddRange(starts.Select(
			(t, i) => content.Substring(t, ends[i] - t)));
		return linesAfter;
	}

	public IEnumerable<SearchResult> SearchContents(string contents)
	{
		var patternMatches = new Dictionary<Regex, int>();
		var results = new List<SearchResult>();
		var newLineIndices = GetNewLineIndices(contents).ToList();
		var startLineIndices = GetStartLineIndices(newLineIndices).ToList();
		var endLineIndices = GetEndLineIndices(contents, newLineIndices).ToList();

		foreach (var p in Settings.SearchPatterns)
		{
			var match = p.Match(contents);
			while (match.Success)
			{
				if (Settings.FirstMatch && patternMatches.ContainsKey(p))
				{
					break;
				}
				var matchIndex = match.Index;
				// get the start and end indices before the match index
				var beforeStartIndices = startLineIndices.TakeWhile(i => i <= matchIndex).ToList();
				var beforeEndIndices = endLineIndices.TakeWhile(i => i < matchIndex).ToList();
				// add another end line index if it exists or the last index of the string
				if (endLineIndices.Count > beforeEndIndices.Count)
					beforeEndIndices.ToList().Add(endLineIndices[beforeEndIndices.Count]);
				else
					beforeEndIndices.ToList().Add(contents.Length - 1);
				var afterStartIndices = startLineIndices.SkipWhile(i => i <= matchIndex).ToList();
				var afterEndIndices = endLineIndices.SkipWhile(i => i <= matchIndex).ToList();
				var lineNum = beforeStartIndices.ToList().Count;
				var startLineIndex = beforeStartIndices.Max();
				var endLineIndex = afterStartIndices.Min() - 1;
				var line = contents.Substring(startLineIndex, endLineIndex - startLineIndex);
				var linesBefore = GetLinesBeforeFromContents(contents,
					beforeStartIndices.AsEnumerable().Reverse().Skip(1).Reverse(),
					beforeEndIndices).ToList();
				var linesAfter = GetLinesAfterFromContents(contents, afterStartIndices,
					afterEndIndices).ToList();
				if ((linesBefore.ToList().Count == 0 || LinesBeforeMatch(linesBefore))
				    &&
				    (linesAfter.ToList().Count == 0 || LinesAfterMatch(linesAfter)))
					results.Add(new SearchResult(
						p,
						null,
						lineNum,
						match.Index - startLineIndex + 1,
						match.Index - startLineIndex + match.Length + 1,
						line,
						new List<string>(linesBefore),
						new List<string>(linesAfter)));
				patternMatches[p] = 1;

				match = match.NextMatch();
			}
		}
		return results;
	}

	private static bool AnyMatchesAnyPattern(IEnumerable<string> strings,
		IEnumerable<Regex> patterns)
	{
		return strings.Any(s => MatchesAnyPattern(s, patterns));
	}

	private static bool MatchesAnyPattern(string s, IEnumerable<Regex> patterns)
	{
		return !string.IsNullOrEmpty(s) && patterns.Any(p => p.Match(s).Success);
	}

	public IEnumerable<SearchResult> SearchLines(IEnumerable<string> lines)
	{
		var patternMatches = new Dictionary<Regex, int>();
		var results = new List<SearchResult>();
		var lineNum = 0;
		var linesBefore = new Queue<string>();
		var linesAfter = new Queue<string>();

		using var lineEnumerator = lines.GetEnumerator();
		while (linesAfter.Count > 0 || lineEnumerator.MoveNext())
		{
			lineNum++;
			var line = linesAfter.Count > 0 ? linesAfter.Dequeue() : lineEnumerator.Current;
			if (Settings.LinesAfter > 0)
			{
				while (linesAfter.Count < Settings.LinesAfter && lineEnumerator.MoveNext())
				{
					linesAfter.Enqueue(lineEnumerator.Current);
				}
			}

			if ((Settings.LinesBefore == 0 || linesBefore.Count == 0 || LinesBeforeMatch(linesBefore))
			    &&
			    (Settings.LinesAfter == 0 || linesAfter.Count == 0 || LinesAfterMatch(linesAfter)))
			{
				foreach (var p in Settings.SearchPatterns)
				{
					var matches = new List<Match>();
					if (Settings.FirstMatch)
					{
						var match = p.Match(line);
						if (match.Success)
						{
							matches.Add(match);
							patternMatches[p] = 1;
						}
					}
					else
					{
						var mc = p.Matches(line);
						if (mc.Count > 0)
						{
							matches.AddRange(mc);
							patternMatches[p] = 1;
						}
					}
					foreach (var match in matches)
					{
						results.Add(new SearchResult(p,
							null,
							lineNum,
							match.Index + 1,
							match.Index + match.Length + 1,
							line,
							new List<string>(linesBefore),
							new List<string>(linesAfter)));
					}
				}
			}

			// If all search patterns are in patternMatches, FirstMatch complete, return results
			if (Settings.FirstMatch && patternMatches.Count == Settings.SearchPatterns.Count)
			{
				return results;
			}

			if (Settings.LinesBefore == 0) continue;
			if (linesBefore.Count == Settings.LinesBefore)
			{
				linesBefore.Dequeue();
			}
			if (linesBefore.Count < Settings.LinesBefore)
			{
				linesBefore.Enqueue(line);
			}
		}

		return results;
	}

	private void SearchTextFileLines(FileResult f)
	{
		try
		{
			var enumerableLines = File.ReadLines(f.FullName);
			var results = SearchLines(enumerableLines);

			foreach (var r in results)
			{
				r.File = f;
				AddSearchResult(r);
			}
		}
		catch (IOException e)
		{
			Logger.Log(e.Message);
		}
	}

	private void SearchTextFileContents(FileResult f)
	{
		try
		{
			var contents = FileUtil.GetFileContents(f.FullName, TextFileEncoding);
			var results = SearchContents(contents);
			foreach (var r in results)
			{
				r.File = f;
				AddSearchResult(r);
			}
		}
		catch (IOException e)
		{
			Logger.Log(e.Message);
		}
	}

	private void SearchTextFile(FileResult f)
	{
		if (Settings.Debug)
			Logger.Log($"Searching text file {f.PathAndName}");
		if (Settings.MultiLineSearch)
			SearchTextFileContents(f);
		else
			SearchTextFileLines(f);
	}

	private void SearchBinaryFile(FileResult fr)
	{
		if (Settings.Verbose)
			Logger.Log($"Searching binary file {fr.PathAndName}");
		try
		{
			using var sr = new StreamReader(fr.FullName, _binaryEncoding);
			var contents = sr.ReadToEnd();
			foreach (var p in Settings.SearchPatterns)
			{
				if (Settings.FirstMatch)
				{
					var m = p.Match(contents);
					if (m.Success)
					{
						AddSearchResult(new SearchResult(
							p,
							fr,
							0,
							m.Index + 1,
							m.Index + m.Length + 1,
							null));
					}
				}
				else
				{
					var matches = p.Matches(contents).Cast<Match>();
					foreach (var m in matches)
					{
						AddSearchResult(new SearchResult(
							p,
							fr,
							0,
							m.Index + 1,
							m.Index + m.Length + 1,
							null));
					}
				}
			}
		}
		catch (IOException e)
		{
			Logger.Log(e.Message);
		}
	}

	public void SearchFile(FileResult f)
	{
		switch (f.Type)
		{
			case FileType.Code:
			case FileType.Text:
			case FileType.Xml:
				SearchTextFile(f);
				break;
			case FileType.Audio:
			case FileType.Binary:
			case FileType.Font:
			case FileType.Image:
			case FileType.Video:
				SearchBinaryFile(f);
				break;
			case FileType.Archive:
				Logger.Log($"Skipping archive file {f.PathAndName}");
				break;
			default:
			{
				if (Settings.Verbose)
				{
					Logger.Log($"Skipping file {f.PathAndName}");
				}

				break;
			}
		}
	}

	private void SearchBatchConcurrent(IReadOnlyList<FileResult> searchFiles)
	{
		var searchTasks = new Task[searchFiles.Count];
		for (var i = 0; i < searchFiles.Count; i++)
		{
			var searchFile = searchFiles[i];
			searchTasks[i] = Task.Factory.StartNew(() => SearchFile(searchFile));
		}
		Task.WaitAll(searchTasks);
	}

	private void SearchBatch(IReadOnlyList<FileResult> findFiles)
	{
		if (findFiles.Count > 100)
		{
			SearchBatchConcurrent(findFiles);
		}
		else
		{
			foreach (var f in findFiles)
			{
				SearchFile(f);
			}
		}
	}

	public List<SearchResult> Search()
	{
		var fileResults = _finder.Find().ToList();

		if (Settings.Verbose)
		{
			var findDirs = fileResults
				.Where(fr => fr.FilePath.Parent != null)
				.Select(fr => fr.FilePath.Parent!.ToString())
				.Distinct()
				.OrderBy(d => d).ToArray();
			Logger.Log($"Directories to be searched ({findDirs.Length}):");
			foreach (var d in findDirs)
			{
				Logger.Log(d);
			}
				
			Logger.Log($"\nFiles to be searched ({fileResults.Count}):");
			foreach (var f in fileResults)
			{
				Logger.Log(f.PathAndName);
			}
		}

		var searched = 0;
		while (fileResults.Count - searched > FileBatchSize)
		{
			SearchBatch(fileResults.Skip(searched).Take(FileBatchSize).ToArray());
			searched += FileBatchSize;
		}
		if (fileResults.Count > searched)
		{
			SearchBatch(fileResults.Skip(searched).ToArray());
		}

		var results = Results.ToList();
		SortSearchResults(results);
		return results;
	}

	private Comparison<SearchResult> GetSearchResultsComparison()
	{
		if (Settings.SortDescending)
		{
			return Settings.SortBy switch
			{
				SortBy.FileName => (r1, r2) => r2.CompareByName(r1, Settings.SortCaseInsensitive),
				SortBy.FileSize => (r1, r2) => r2.CompareBySize(r1, Settings.SortCaseInsensitive),
				SortBy.FileType => (r1, r2) => r2.CompareByType(r1, Settings.SortCaseInsensitive),
				SortBy.LastMod => (r1, r2) => r2.CompareByLastMod(r1, Settings.SortCaseInsensitive),
				_ => (r1, r2) => r2.CompareByPath(r1, Settings.SortCaseInsensitive)
			};
		}

		return Settings.SortBy switch
		{
			SortBy.FileName => (r1, r2) => r1.CompareByName(r2, Settings.SortCaseInsensitive),
			SortBy.FileSize => (r1, r2) => r1.CompareBySize(r2, Settings.SortCaseInsensitive),
			SortBy.FileType => (r1, r2) => r1.CompareByType(r2, Settings.SortCaseInsensitive),
			SortBy.LastMod => (r1, r2) => r1.CompareByLastMod(r2, Settings.SortCaseInsensitive),
			_ => (r1, r2) => r1.CompareByPath(r2, Settings.SortCaseInsensitive)
		};
	}

	private void SortSearchResults(List<SearchResult> results)
	{
		var comparison = GetSearchResultsComparison();
		results.Sort(comparison);

		if (Settings.SortDescending)
		{
			results.Reverse();
		}
	}

	public static void PrintResults(IEnumerable<SearchResult> results, SearchResultFormatter formatter)
	{
		// File sorting is done by CsFind, so maybe additional sorting isn't needed?
		// var sortedResults = GetSortedSearchResults();
		// var formatter = new SearchResultFormatter(Settings);
		var resultsList = results.ToList();
		Logger.Log($"Search results ({resultsList.Count}):");
		foreach (var searchResult in resultsList)
		{
			Logger.Log(formatter.Format(searchResult));
		}
	}

	private static IEnumerable<FileResult> GetMatchingFiles(IEnumerable<SearchResult> results)
	{
		return
		[
			..results.Where(r => r.File != null)
				.Select(r => r.File!)
				.DistinctBy(fp => fp.ToString())
		];
	}

	public static void PrintMatchingDirs(IEnumerable<SearchResult> results, SearchResultFormatter formatter)
	{
		var matchingFiles = GetMatchingFiles(results);
		Finder.PrintMatchingDirs(matchingFiles, formatter.FileFormatter);
	}

	public static void PrintMatchingFiles(IEnumerable<SearchResult> results, SearchResultFormatter formatter)
	{
		var matchingFiles = GetMatchingFiles(results);
		Finder.PrintMatchingFiles(matchingFiles, formatter.FileFormatter);
	}

	private List<string> GetMatchingLines(IEnumerable<SearchResult> results)
	{
		var lines = results.Where(r => r.Line != null)
			.Select(r => r.Line!.Trim()).ToList();
		if (Settings.UniqueLines)
		{
			lines = new HashSet<string>(lines).ToList();
		}
		lines.Sort(new CaseInsensitiveComparer());
		return lines;
	}

	public void PrintMatchingLines(IEnumerable<SearchResult> results, SearchResultFormatter formatter)
	{
		var matchingLines = GetMatchingLines(results).ToList();
		var hdrText = Settings.UniqueLines ? "Unique matching lines" : "Matching lines";
		Logger.Log($"\n{hdrText} ({matchingLines.Count}):");
		foreach (var m in matchingLines)
		{
			Logger.Log(formatter.FormatLine(m));
		}
	}
}

internal class CaseInsensitiveComparer : IComparer<string>
{
	public int Compare(string? a, string? b)
	{
		return a switch
		{
			null when b == null => 0,
			null => -1,
			not null when b == null => 1,
			_ => string.Compare(a.ToUpper(), b.ToUpper(), StringComparison.Ordinal)
		};
	}
}
