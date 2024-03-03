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
		ValidateSettings();
		_finder = new Finder(settings);
		Results = new ConcurrentBag<SearchResult>();
	}

	private void ValidateSettings()
	{
		if (Settings.Paths.Count == 0)
			throw new SearchException("Startpath not defined");
		if (Settings.Paths.Select(FileUtil.ExpandPath).Any(p => !Directory.Exists(p) && !File.Exists(p)))
		{
			throw new SearchException("Startpath not found");
		}
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

	public void Search()
	{
		var fileResults = _finder.Find().ToList();

		if (Settings.Verbose)
		{
			var findDirs = fileResults
				.Where(sf => sf.File.Directory != null)
				.Select(sf => sf.File.Directory!.ToString())
				.Distinct()
				.OrderBy(d => d).ToArray();
			Logger.Log($"Directories to be searched ({findDirs.Length}):");
			foreach (var d in findDirs)
			{
				Logger.Log(FileUtil.ContractOrRelativePath(d, Settings.Paths));
			}
				
			Logger.Log($"\nFiles to be searched ({fileResults.Count}):");
			foreach (var f in fileResults)
			{
				Logger.Log(FileUtil.ContractOrRelativePath(f.FullName, Settings.Paths));
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

	public void SearchFile(FileResult f)
	{
		switch (f.Type)
		{
			case FileType.Code:
			case FileType.Text:
			case FileType.Xml:
				SearchTextFile(f);
				break;
			case FileType.Binary:
				SearchBinaryFile(f);
				break;
			case FileType.Archive:
				Logger.Log($"Skipping archive file {FileUtil.ContractPath(f.FullName)}");
				break;
			default:
			{
				if (Settings.Verbose)
				{
					Logger.Log($"Skipping file {FileUtil.ContractPath(f.FullName)}");
				}

				break;
			}
		}
	}

	private void SearchTextFile(FileResult f)
	{
		if (Settings.Debug)
			Logger.Log($"Searching text file {FileUtil.ContractOrRelativePath(f.FullName, Settings.Paths)}");
		if (Settings.MultiLineSearch)
			SearchTextFileContents(f);
		else
			SearchTextFileLines(f);
	}

	private void SearchTextFileContents(FileResult f)
	{
		try
		{
			var contents = FileUtil.GetFileContents(f, TextFileEncoding);
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

	private void SearchTextFileLines(FileResult f)
	{
		try
		{
			var enumerableLines = FileUtil.EnumerableStringFromFile(f, TextFileEncoding);
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

	private static bool AnyMatchesAnyPattern(IEnumerable<string> strings,
		IEnumerable<Regex> patterns)
	{
		return strings.Any(s => MatchesAnyPattern(s, patterns));
	}

	private static bool MatchesAnyPattern(string s, IEnumerable<Regex> patterns)
	{
		return !string.IsNullOrEmpty(s) && patterns.Any(p => p.Match(s).Success);
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

	public IEnumerable<SearchResult> SearchLines(IEnumerable<string> lines)
	{
		var patternMatches = new Dictionary<Regex, int>();
		var results = new List<SearchResult>();
		var lineNum = 0;
		var linesBefore = new Queue<string>();
		var linesAfter = new Queue<string>();

		using var lineEnumerator = lines.GetEnumerator();
		var stop = false;
		while ((lineEnumerator.MoveNext() || linesAfter.Count > 0) && !stop)
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
					foreach (var match in p.Matches(line).Where(m => m != null))
					{
						if (Settings.FirstMatch && patternMatches.ContainsKey(p))
						{
							stop = true;
							break;
						}
						results.Add(new SearchResult(p,
							null,
							lineNum,
							match.Index + 1,
							match.Index + match.Length + 1,
							line,
							new List<string>(linesBefore),
							new List<string>(linesAfter)));
						patternMatches[p] = 1;
					}
				}
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

	private void SearchBinaryFile(FileResult sf)
	{
		if (Settings.Verbose)
			Logger.Log($"Searching binary file {FileUtil.ContractPath(sf.FullName)}");
		try
		{
			using var sr = new StreamReader(sf.FullName, _binaryEncoding);
			var contents = sr.ReadToEnd();
			foreach (var p in Settings.SearchPatterns)
			{
				var matches = p.Matches(contents).Cast<Match>();
				if (Settings.FirstMatch)
				{
					matches = matches.Take(1);
				}
				foreach (var m in matches)
				{
					AddSearchResult(new SearchResult(
						p,
						sf,
						0,
						m.Index + 1,
						m.Index + m.Length + 1,
						null));
				}
			}
		}
		catch (IOException e)
		{
			Logger.Log(e.Message);
		}
	}

	private void AddSearchResult(SearchResult searchResult)
	{
		Results.Add(searchResult);
	}

	private IList<SearchResult> GetSortedSearchResults()
	{
		var sorted = Results.ToList();
		sorted.Sort(new SearchResultsComparer());
		return sorted;
	}

	public void PrintResults()
	{
		var sortedResults = GetSortedSearchResults();
		var formatter = new SearchResultFormatter(Settings);
		Logger.Log($"Search results ({sortedResults.Count}):");
		foreach (var searchResult in sortedResults)
		{
			Logger.Log(formatter.Format(searchResult));
		}
	}

	private IEnumerable<DirectoryInfo> GetMatchingDirs()
	{
		return new List<DirectoryInfo>(
			Results.Where(r => r.File?.File.Directory != null)
				.Select(r => r.File!.File.Directory!)
  				.Distinct()
				.OrderBy(d => d.FullName));
	}

	public void PrintMatchingDirs()
	{
		var matchingDirs = GetMatchingDirs()
			.Select(d => FileUtil.GetRelativePath(d.FullName, Settings.Paths))
			.Distinct()
			.OrderBy(d => d).ToList();
		Logger.Log($"\nDirectories with matches ({matchingDirs.Count}):");
		foreach (var d in matchingDirs)
		{
			Logger.Log(d);
		}
	}

	private IEnumerable<FileInfo> GetMatchingFiles()
	{
		return new List<FileInfo>(
			Results.Where(r => r.File != null)
				.Select(r => r.File!.PathAndName)
				.Distinct().Select(f => new FileInfo(f))
				.OrderBy(d => d.FullName));
	}

	public void PrintMatchingFiles()
	{
		var matchingFiles = GetMatchingFiles()
			.Select(f => FileUtil.GetRelativePath(f.FullName, Settings.Paths))
			.Distinct()
			.OrderBy(f => f).ToList();
		Logger.Log($"\nFiles with matches ({matchingFiles.Count()}):");
		foreach (var f in matchingFiles)
		{
			Logger.Log(f);
		}
	}

	private IEnumerable<string> GetMatchingLines()
	{
		var lines = Results.Where(r => r.Line != null)
			.Select(r => r.Line!.Trim()).ToList();
		if (Settings.UniqueLines)
		{
			lines = new HashSet<string>(lines).ToList();
		}
		lines.Sort(new CaseInsensitiveComparer());
		return lines;
	}

	public void PrintMatchingLines()
	{
		var matchingLines = GetMatchingLines().ToList();
		var hdrText = Settings.UniqueLines ? "Unique lines with matches" : "Lines with matches";
		Logger.Log($"\n{hdrText} ({matchingLines.Count()}):");
		foreach (var m in matchingLines)
		{
			Logger.Log(m);
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
