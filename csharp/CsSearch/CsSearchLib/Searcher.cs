using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace CsSearchLib
{
	public class Searcher
	{
		private readonly FileTypes _fileTypes;
		private SearchSettings Settings { get; set; }
		private ConcurrentBag<SearchResult> Results { get; set; }
		private Encoding TextFileEncoding { get; set; } = Encoding.Default;

		// TODO: move this to SearchSettings
		private const int FileBatchSize = 255;
		// use single-byte encoding for reading binary files (will break if UTF8)
		private readonly Encoding _binaryEncoding = Encoding.GetEncoding("ISO-8859-1");

		public Searcher(SearchSettings settings)
		{
			Settings = settings;
			ValidateSettings();
			_fileTypes = new FileTypes();
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

		public bool IsSearchDirectory(DirectoryInfo d)
		{
			if (FileUtil.IsDotDir(d.Name))
				return true;
			if (Settings.ExcludeHidden && FileUtil.IsHidden(d))
				return false;
			return (Settings.InDirPatterns.Count == 0 ||
			        Settings.InDirPatterns.Any(p => p.Matches(d.FullName).Count > 0)) &&
			       (Settings.OutDirPatterns.Count == 0 ||
			        !Settings.OutDirPatterns.Any(p => p.Matches(d.FullName).Count > 0));
		}

		public bool IsSearchFile(SearchFile sf)
		{
			return (Settings.InExtensions.Count == 0 ||
			        Settings.InExtensions.Contains(sf.File.Extension)) &&
			       (Settings.OutExtensions.Count == 0 ||
			        !Settings.OutExtensions.Contains(sf.File.Extension)) &&
			       (Settings.InFilePatterns.Count == 0 ||
			        Settings.InFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.OutFilePatterns.Count == 0 ||
			        !Settings.OutFilePatterns.Any(p => p.Match(sf.File.Name).Success));
		}

		public bool IsArchiveSearchFile(SearchFile sf)
		{
			return (Settings.InArchiveExtensions.Count == 0 ||
			        Settings.InArchiveExtensions.Contains(sf.File.Extension)) &&
			       (Settings.OutArchiveExtensions.Count == 0 ||
			        !Settings.OutArchiveExtensions.Contains(sf.File.Extension)) &&
			       (Settings.InArchiveFilePatterns.Count == 0 ||
			        Settings.InArchiveFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.OutArchiveFilePatterns.Count == 0 ||
			        !Settings.OutArchiveFilePatterns.Any(p => p.Match(sf.File.Name).Success));
		}

		public bool FilterFile(SearchFile sf)
		{
			if (FileUtil.IsHidden(sf.File) && Settings.ExcludeHidden)
				return false;
			if (sf.Type.Equals(FileType.Archive))
			{
				return (Settings.SearchArchives && IsArchiveSearchFile(sf));
			}
			return (!Settings.ArchivesOnly && IsSearchFile(sf));
		}

		private IEnumerable<SearchFile> GetSearchFilesForPath(string path)
		{
			var searchFiles = new List<SearchFile>();
			var searchOption = Settings.Recursive ? System.IO.SearchOption.AllDirectories :
				System.IO.SearchOption.TopDirectoryOnly;

			var expandedPath = FileUtil.ExpandPath(path);
			if (Directory.Exists(expandedPath))
			{
				searchFiles.AddRange(new DirectoryInfo(expandedPath).
					EnumerateFiles("*", searchOption).
					Where(f => f.Directory == null || IsSearchDirectory(f.Directory)).
					Select(f => new SearchFile(f, _fileTypes.GetFileType(f))).
					Where(FilterFile).
					Select(sf => sf));
			}
			else if (File.Exists(expandedPath))
			{
				var fi = new FileInfo(expandedPath);
				var ff = new SearchFile(fi, _fileTypes.GetFileType(fi));
				if (FilterFile(ff))
				{
					searchFiles.Add(ff);
				}
			}

			return searchFiles;
		}

		// private IEnumerable<SearchFile> GetSearchFiles()
		// {
		// 	var searchFiles = new List<SearchFile>();
		// 	foreach (var p in Settings.Paths)
		// 	{
		// 		searchFiles.AddRange(GetSearchFilesForPath(p));
		// 	}
		//
		// 	return searchFiles;
		// }

		public void Search()
		{
			foreach (var p in Settings.Paths)
			{
				var expandedPath = FileUtil.ExpandPath(p);
				if (Directory.Exists(expandedPath))
				{
					if (IsSearchDirectory(new DirectoryInfo(expandedPath)))
					{
						SearchPath(p);
					}
					else
					{
						throw new SearchException("Startpath does not match search settings");
					}
				}
				else if (File.Exists(expandedPath))
				{
					var f = new FileInfo(expandedPath);
					var sf = new SearchFile(f, _fileTypes.GetFileType(f));
					if (FilterFile(sf))
					{
						SearchFile(sf);
					}
					else
					{
						throw new SearchException("Startpath does not match search settings");
					}
				}
				else
				{
					throw new SearchException($"Path not found: {p}");
				}
			}
		}

		public void SearchPath(string path)
		{
			var searchFiles = GetSearchFilesForPath(path).ToList();
			if (Settings.Verbose)
			{
				searchFiles.Sort(new SearchFilesComparer());

				var searchDirs = searchFiles
					.Where(sf => sf.File.Directory != null)
					.Select(sf => sf.File.Directory!.ToString())
					.Distinct()
					.OrderBy(d => d).ToArray();
				Common.Log($"Directories to be searched ({searchDirs.Length}):");
				foreach (var d in searchDirs)
				{
					Common.Log(FileUtil.ContractOrRelativePath(d, path));
				}
				
				Common.Log($"\nFiles to be searched ({searchFiles.Count}):");
				foreach (var f in searchFiles)
				{
					Common.Log(FileUtil.ContractOrRelativePath(f.FullName, path));
				}
			}

			var searched = 0;
			while (searchFiles.Count - searched > FileBatchSize)
			{
				SearchBatch(searchFiles.Skip(searched).Take(FileBatchSize).ToArray());
				searched += FileBatchSize;
			}
			if (searchFiles.Count > searched)
			{
				SearchBatch(searchFiles.Skip(searched).ToArray());
			}
		}

		private void SearchBatch(IReadOnlyList<SearchFile> searchFiles)
		{
			if (searchFiles.Count > 100)
			{
				SearchBatchConcurrent(searchFiles);
			}
			else
			{
				foreach (var f in searchFiles)
				{
					SearchFile(f);
				}
			}
		}

		private void SearchBatchConcurrent(IReadOnlyList<SearchFile> searchFiles)
		{
			var searchTasks = new Task[searchFiles.Count];
			for (var i = 0; i < searchFiles.Count; i++)
			{
				var searchFile = searchFiles[i];
				searchTasks[i] = Task.Factory.StartNew(() => SearchFile(searchFile));
			}
			Task.WaitAll(searchTasks);
		}

		public void SearchFile(SearchFile f)
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
					Common.Log($"Skipping archive file {FileUtil.ContractPath(f.FullName)}");
					break;
				default:
				{
					if (Settings.Verbose)
					{
						Common.Log($"Skipping file {FileUtil.ContractPath(f.FullName)}");
					}

					break;
				}
			}
		}

		private void SearchTextFile(SearchFile f)
		{
			if (Settings.Debug)
				// Common.Log($"Searching text file {FileUtil.ContractOrRelativePath(f.FullName, Settings.StartPath!)}");
				Common.Log($"Searching text file {f}");
			if (Settings.MultiLineSearch)
				SearchTextFileContents(f);
			else
				SearchTextFileLines(f);
		}

		private void SearchTextFileContents(SearchFile f)
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
				Common.Log(e.Message);
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
			var newLineIndices = GetNewLineIndices(contents);
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
						beforeEndIndices);
					var linesAfter = GetLinesAfterFromContents(contents, afterStartIndices,
						afterEndIndices);
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

		private void SearchTextFileLines(SearchFile f)
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
				Common.Log(e.Message);
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
			return ((inPatterns.Count == 0 || AnyMatchesAnyPattern(lines, inPatterns))
				&& (outPatterns.Count == 0 || !AnyMatchesAnyPattern(lines, outPatterns)));
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
						foreach (Match match in p.Matches(line).Where(m => m != null))
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

		private void SearchBinaryFile(SearchFile sf)
		{
			if (Settings.Verbose)
				Common.Log($"Searching binary file {FileUtil.ContractPath(sf.FullName)}");
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
				Common.Log(e.Message);
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
			Common.Log($"Search results ({sortedResults.Count}):");
			foreach (var searchResult in sortedResults)
			{
				Common.Log(formatter.Format(searchResult));
			}
		}

		private IEnumerable<DirectoryInfo> GetMatchingDirs()
		{
			return new List<DirectoryInfo>(
				Results.Where(r => r.File?.File.Directory != null)
					.Select(r => r.File!.File.Directory)
					.Distinct()
					.OrderBy(d => d.FullName));
		}

		public void PrintMatchingDirs()
		{
			var matchingDirs = GetMatchingDirs()
				// .Select(d => FileUtil.GetRelativePath(d.FullName, Settings.StartPath!))
				.Select(d => d.ToString())
				.Distinct()
				.OrderBy(d => d).ToList();
			Common.Log($"\nDirectories with matches ({matchingDirs.Count()}):");
			foreach (var d in matchingDirs)
			{
				Common.Log(d);
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
				// .Select(f => FileUtil.GetRelativePath(f.FullName, Settings.StartPath!))
				.Select(f => f.ToString())
				.Distinct()
				.OrderBy(f => f).ToList();
			Common.Log($"\nFiles with matches ({matchingFiles.Count()}):");
			foreach (var f in matchingFiles)
			{
				Common.Log(f);
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
			Common.Log($"\n{hdrText} ({matchingLines.Count()}):");
			foreach (var m in matchingLines)
			{
				Common.Log(m);
			}
		}
	}

	internal class CaseInsensitiveComparer : IComparer<string>
	{
		public int Compare(string? a, string? b)
		{
			if (a == null && b == null)
				return 0;
			if (a == null)
				return -1;
			if (b == null)
				return 1;
			return string.Compare(a.ToUpper(), b.ToUpper(), StringComparison.Ordinal);
		}
	}
}
