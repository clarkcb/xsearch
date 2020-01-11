using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace CsSearch
{
	public class Searcher
	{
		private readonly FileTypes _fileTypes;
		public SearchSettings Settings { get; private set; }
		public ConcurrentBag<SearchResult> Results { get; private set; }
		private Encoding TextFileEncoding { get; set; }

		private const int FileBatchSize = 255;

		public Searcher(SearchSettings settings)
		{
			Settings = settings;
			ValidateSettings();
			_fileTypes = new FileTypes();
			Results = new ConcurrentBag<SearchResult>();
		}

		private void ValidateSettings()
		{
			if (string.IsNullOrEmpty(Settings.StartPath))
				throw new SearchException("Startpath not defined");
			var expandedPath = FileUtil.ExpandPath(Settings.StartPath);
			if (FileUtil.IsDirectory(expandedPath))
			{
				if (!(new DirectoryInfo(expandedPath)).Exists)
					throw new SearchException("Startpath not found");
			}
			else
			{
				if (!(new FileInfo(expandedPath)).Exists)
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
		}

		public bool IsSearchDirectory(DirectoryInfo d)
		{
			if (FileUtil.IsDotDir(d.Name))
				return true;
			if (Settings.ExcludeHidden && FileUtil.IsHidden(d))
				return false;
			if (Settings.InDirPatterns.Count > 0 &&
				!Settings.InDirPatterns.Any(p => p.Match(d.Name).Success))
				return false;
			if (Settings.OutDirPatterns.Count > 0 &&
				Settings.OutDirPatterns.Any(p => p.Match(d.Name).Success))
				return false;
			return true;
		}

		public bool IsSearchFile(FileInfo f)
		{
			if (Settings.InExtensions.Count > 0 &&
				!Settings.InExtensions.Contains(f.Extension))
				return false;
			if (Settings.OutExtensions.Count > 0 &&
				Settings.OutExtensions.Contains(f.Extension))
				return false;
			if (Settings.InFilePatterns.Count > 0 &&
				!Settings.InFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			if (Settings.OutFilePatterns.Count > 0 &&
				Settings.OutFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			return true;
		}

		public bool IsArchiveSearchFile(FileInfo f)
		{
			if (Settings.InArchiveExtensions.Count > 0 &&
				!Settings.InArchiveExtensions.Contains(f.Extension))
				return false;
			if (Settings.OutArchiveExtensions.Count > 0 &&
				Settings.OutArchiveExtensions.Contains(f.Extension))
				return false;
			if (Settings.InArchiveFilePatterns.Count > 0 &&
				!Settings.InArchiveFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			if (Settings.OutArchiveFilePatterns.Count > 0 &&
				Settings.OutArchiveFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			return true;
		}

		public IEnumerable<DirectoryInfo> GetSearchDirs(DirectoryInfo dir)
		{
			IEnumerable<DirectoryInfo> searchDirs = new List<DirectoryInfo>();
			try
			{
				searchDirs = dir.EnumerateDirectories().Where(IsSearchDirectory);
				return searchDirs.Aggregate(searchDirs, (current, d) => current.Concat(GetSearchDirs(d)));
			}
			catch (IOException e)
			{
				if (Settings.Verbose)
					Common.Log($"Error while accessing dir {FileUtil.ContractPath(dir.FullName)}: {e.Message}");
			}
			return searchDirs;
		}

		public bool FilterFile(FileInfo f)
		{
			if (FileUtil.IsHidden(f) && Settings.ExcludeHidden)
				return false;
			if (_fileTypes.IsArchiveFile(f))
			{
				return (Settings.SearchArchives && IsArchiveSearchFile(f));
			}
			return (!Settings.ArchivesOnly && IsSearchFile(f));
		}

		private SearchFile SearchFileFromFileInfo(FileInfo f)
		{
			return new SearchFile(new List<string>(), f.DirectoryName, f.Name, _fileTypes.GetFileType(f));
		}

		private IEnumerable<SearchFile> GetSearchFilesForDir(DirectoryInfo dir)
		{
			if (Settings.Debug)
			{
				Common.Log($"Getting search files under {FileUtil.ContractPath(dir.FullName)}");
			}
			IEnumerable<SearchFile> dirSearchFiles = new List<SearchFile>();
			try
			{
				dirSearchFiles = dir.EnumerateFiles().
					Where(FilterFile).
					Select((f,i) => SearchFileFromFileInfo(f));
			}
			catch (IOException e)
			{
				if (Settings.Verbose)
					Common.Log($"Error while accessing dir {FileUtil.ContractPath(dir.FullName)}: {e.Message}");
			}
			return dirSearchFiles;
		}

		public IEnumerable<SearchFile> GetSearchFiles(IEnumerable<DirectoryInfo> dirs)
		{
			var searchFiles = new List<SearchFile>();
			foreach (var d in dirs)
			{
				searchFiles.AddRange(GetSearchFilesForDir(d));
			}
			return searchFiles;
		}

		public void Search()
		{
			var expandedPath = FileUtil.ExpandPath(Settings.StartPath);
			if (FileUtil.IsDirectory(expandedPath))
			{
				var startDir = new DirectoryInfo(expandedPath);
				if (IsSearchDirectory(startDir))
				{
					SearchPath(startDir);
				}
				else
				{
					throw new SearchException("Startpath does not match search settings");
				}
			}
			else
			{
				var f = new FileInfo(expandedPath);
				if (FilterFile(f))
				{
					DoSearchFile(new SearchFile(f.DirectoryName, f.Name, _fileTypes.GetFileType(f)));
				}
				else
				{
					throw new SearchException("Startpath does not match search settings");
				}
			}
		}

		public void SearchPath(DirectoryInfo path)
		{
			var searchDirs = new List<DirectoryInfo> { path };
			if (Settings.Recursive)
			{
				searchDirs.AddRange(GetSearchDirs(path));
			}
			if (Settings.Verbose)
			{
				Common.Log($"Directories to be searched ({searchDirs.Count}):");
				foreach (var d in searchDirs)
				{
					Common.Log(FileUtil.ContractPath(d.FullName));
				}
				Common.Log("");
			}

			var searchFiles = GetSearchFiles(searchDirs).ToArray();
			if (Settings.Verbose)
			{
				Common.Log($"\nFiles to be searched ({searchFiles.Length}):");
				foreach (var f in searchFiles)
				{
					Common.Log(FileUtil.ContractPath(f.FullName));
				}
				Common.Log("");
			}

			var searched = 0;
			while (searchFiles.Length - searched > FileBatchSize)
			{
				SearchBatch(searchFiles.Skip(searched).Take(FileBatchSize).ToArray());
				searched += FileBatchSize;
			}
			if (searchFiles.Length > searched)
			{
				SearchBatch(searchFiles.Skip(searched).ToArray());
			}
		}

		private void SearchBatch(SearchFile[] searchFiles)
		{
			if (searchFiles.Length > 100)
			{
				SearchBatchConcurrent(searchFiles);
			}
			else
			{
				foreach (var f in searchFiles)
				{
					DoSearchFile(f);
				}
			}
		}

		private void SearchBatchConcurrent(SearchFile[] searchFiles)
		{
			var searchTasks = new Task[searchFiles.Length];
			for (var i = 0; i < searchFiles.Length; i++)
			{
				var searchFile = searchFiles[i];
				searchTasks[i] = Task.Factory.StartNew(() => DoSearchFile(searchFile));
			}
			Task.WaitAll(searchTasks);
		}

		public void DoSearchFile(SearchFile f)
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
			if (Settings.Verbose)
				Common.Log($"Searching text file {FileUtil.ContractPath(f.FullName)}");
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
			if (Settings.LinesBefore > 0)
			{
				var starts = beforeStartIndices.Reverse().Take(Settings.LinesBefore).Reverse().ToList();
				var ends = beforeEndIndices.Reverse().Take(Settings.LinesBefore + 1).Reverse().Skip(1).ToList();
				linesBefore.AddRange(starts.Select((t, i) => content.Substring(t, ends[i] - t)));
			}
			return linesBefore;
		}

		private IEnumerable<string> GetLinesAfterFromContents(string content,
			IEnumerable<int> afterStartIndices, IEnumerable<int> afterEndIndices)
		{
			var linesAfter = new List<string>();
			if (Settings.LinesAfter > 0)
			{
				var starts = afterStartIndices.Take(Settings.LinesAfter).ToList();
				var ends = afterEndIndices.Skip(1).Take(Settings.LinesAfter).ToList();
				linesAfter.AddRange(starts.Select((t, i) => content.Substring(t, ends[i] - t)));
			}
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
					var linesAfter = GetLinesAfterFromContents(contents, afterStartIndices, afterEndIndices);
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
			var lineEnumerator = lines.GetEnumerator();
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
						var matches = p.Matches(line);
						foreach (Match match in matches)
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
				if (Settings.LinesBefore > 0)
				{
					if (linesBefore.Count == Settings.LinesBefore)
					{
						linesBefore.Dequeue();
					}
					if (linesBefore.Count < Settings.LinesBefore)
					{
						linesBefore.Enqueue(line);
					}
				}
			}
			return results;
		}

		private void SearchBinaryFile(SearchFile f)
		{
			if (Settings.Verbose)
				Common.Log($"Searching binary file {FileUtil.ContractPath(f.FullName)}");
			try
			{
				// get the binary bytes in a single-byte encoding (will break if UTF8)
				//using (var sr = new StreamReader(f.FullName, Encoding.GetEncoding("ISO_8859_1")))
				using (var sr = new StreamReader(f.FullName))
				{
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
								f,
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
				Common.Log(e.Message);
			}
		}

		private void AddSearchResult(SearchResult searchResult)
		{
			Results.Add(searchResult);
		}

		private IList<SearchResult> GetSortedSearchResults()
		{
			IComparer<SearchResult> comparer = new SearchResultsComparer();
			var sorted = Results.ToList();
			sorted.Sort(comparer);
			return sorted;
		}

		public void PrintResults()
		{
			var sortedResults = GetSortedSearchResults();
			Common.Log($"Search results ({sortedResults.Count}):");
			foreach (var searchResult in sortedResults)
			{
				Common.Log(searchResult.ToString(Settings));
			}
		}

		public IEnumerable<DirectoryInfo> GetMatchingDirs()
		{
			return new List<DirectoryInfo>(
				Results.Select(r => r.File.FilePath).
				Distinct().Select(d => new DirectoryInfo(d)).
				OrderBy(d => d.FullName));
		}

		public void PrintMatchingDirs()
		{
			var matchingDirs = GetMatchingDirs();
			Common.Log($"\nDirectories with matches ({matchingDirs.Count()}):");
			foreach (var d in matchingDirs)
			{
				Common.Log(FileUtil.GetRelativePath(d.FullName, Settings.StartPath));
			}
		}

		public IEnumerable<FileInfo> GetMatchingFiles()
		{
			return new List<FileInfo>(
				Results.Select(r => r.File.PathAndName).
				Distinct().Select(f => new FileInfo(f)).
				OrderBy(d => d.FullName));
		}

		public void PrintMatchingFiles()
		{
			var matchingFiles = GetMatchingFiles();
			Common.Log($"\nFiles with matches ({matchingFiles.Count()}):");
			foreach (var f in matchingFiles)
			{
				Common.Log(FileUtil.GetRelativePath(f.FullName, Settings.StartPath));
			}
		}

		public IEnumerable<string> GetMatchingLines()
		{
			var lines = Results.Select(r => r.Line.Trim()).ToList();
			if (Settings.UniqueLines)
			{
				lines = new HashSet<string>(lines).ToList();
			}
			lines.Sort(new CaseInsensitiveComparer());
			return lines;
		}

		public void PrintMatchingLines()
		{
			var matchingLines = GetMatchingLines();
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
		public int Compare(string a, string b)
		{
			return a.ToUpper().CompareTo(b.ToUpper());
		}
	}
}
