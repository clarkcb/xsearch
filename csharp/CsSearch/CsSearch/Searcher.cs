using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace CsSearch
{
	public class Searcher
	{
		private readonly FileTypes _fileTypes;
		public SearchSettings Settings { get; private set; }
		public IList<SearchResult> Results { get; private set; }
		public IDictionary<string, Stopwatch> Timers { get; private set; }
		private TimeSpan TotalElapsedTime { get; set; }

		public Searcher(SearchSettings settings)
		{
			Settings = settings;
			if (Settings.Verbose)
				Log(Settings + "\n");
			ValidateSettings();
			_fileTypes = new FileTypes();
			Results = new List<SearchResult>();
			Timers = new Dictionary<string, Stopwatch>();
			TotalElapsedTime = new TimeSpan();
		}

		private static void Log(string message)
		{
			Console.WriteLine(message);
		}

		private void ValidateSettings()
		{
			if (string.IsNullOrEmpty(Settings.StartPath))
				throw new SearchException("Startpath not defined");
			if (Settings.SearchPatterns.Count < 1)
				throw new SearchException("No search patterns specified");
		}

		private bool IsSearchDirectory(DirectoryInfo d)
		{
			if (Settings.ExcludeHidden && FileUtil.IsHiddenFile(d))
				return false;
			if (Settings.InDirPatterns.Count > 0 &&
				!Settings.InDirPatterns.Any(p => p.Match(d.Name).Success))
				return false;
			if (Settings.OutDirPatterns.Count > 0 &&
				Settings.OutDirPatterns.Any(p => p.Match(d.Name).Success))
				return false;
			return true;
		}

		private bool IsSearchFile(FileInfo f)
		{
			if (Settings.ExcludeHidden && FileUtil.IsHiddenFile(f))
				return false;
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

		private bool IsArchiveSearchFile(FileInfo f)
		{
			if (Settings.ExcludeHidden && FileUtil.IsHiddenFile(f))
				return false;
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

		public void StartTimer(string name)
		{
			var timer = new Stopwatch();
			timer.Start();
			Timers.Add(name, timer);
		}

		public void StopTimer(string name) {
			var timer = Timers[name];
			timer.Stop();
			TotalElapsedTime = TotalElapsedTime.Add(timer.Elapsed);
		}

		public TimeSpan GetElapsed(string name)
		{
			var timer = Timers[name];
			return timer.Elapsed;
		}

		public void PrintElapsed(string name)
		{
			var ts = GetElapsed(name);
			Log(string.Format("Elapsed time for {0}: {1} ms", name, ts.TotalMilliseconds));
		}

		public void PrintTotalElapsed()
		{
			Log(string.Format("Total elapsed time: {0} ms", TotalElapsedTime.TotalMilliseconds));
		}

		public IEnumerable<DirectoryInfo> GetSearchDirs(DirectoryInfo startDir)
		{
			var searchDirs = new List<DirectoryInfo>();
			if (IsSearchDirectory(startDir))
			{
				searchDirs.Add(startDir);
			}
			if (Settings.Recursive)
			{
				searchDirs.AddRange(RecGetSearchDirs(startDir));
			}
			return searchDirs;
		}

		private IEnumerable<DirectoryInfo> RecGetSearchDirs(DirectoryInfo dir)
		{
			IEnumerable<DirectoryInfo> searchDirs = new List<DirectoryInfo>();
			try
			{
				searchDirs = dir.EnumerateDirectories().Where(IsSearchDirectory);
				return searchDirs.Aggregate(searchDirs, (current, d) => current.Concat(RecGetSearchDirs(d)));
			}
			catch (IOException e)
			{
				if (Settings.Verbose)
					Log(String.Format("Error while accessing dir {0}: {1}",
						FileUtil.GetRelativePath(dir.FullName), e.Message));
			}
			return searchDirs;
		}
		private bool FilterFile(FileInfo f)
		{
			return
				(_fileTypes.IsArchiveFile(f) && Settings.SearchArchives && IsArchiveSearchFile(f))
				||
				(!Settings.ArchivesOnly && IsSearchFile(f));
		}

		private SearchFile SearchFileFromFileInfo(FileInfo f)
		{
			return new SearchFile(new List<string>(), f.DirectoryName, f.Name, _fileTypes.GetFileType(f));
		}

		private IEnumerable<SearchFile> GetSearchFilesForDir(DirectoryInfo dir)
		{
			if (Settings.Debug)
			{
				Log(string.Format("Getting search files under {0}",
					FileUtil.GetRelativePath(dir.FullName)));
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
					Log(String.Format("Error while accessing dir {0}: {1}",
						FileUtil.GetRelativePath(dir.FullName), e.Message));
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
			var attr = File.GetAttributes(Settings.StartPath);
			if ((attr & FileAttributes.Directory) == FileAttributes.Directory)
			{
				var startDir = new DirectoryInfo(Settings.StartPath);
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
				var f = new FileInfo(Settings.StartPath);
				if (f.Exists && FilterFile(f))
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
			if (Settings.DoTiming)
			{
				StartTimer("GetSearchDirs");
			}
			var startDir = path;
			var searchDirs = new List<DirectoryInfo>();
			searchDirs.AddRange(GetSearchDirs(startDir));
			if (Settings.DoTiming)
			{
				StopTimer("GetSearchDirs");
				if (Settings.PrintResults)
					PrintElapsed("GetSearchDirs");
			}
			if (Settings.Verbose)
			{
				Log(string.Format("Directories to be searched ({0}):", searchDirs.Count));
				foreach (var d in searchDirs)
				{
					Log(FileUtil.GetRelativePath(d.FullName));
				}
				Log("");
			}

			if (Settings.DoTiming)
			{
				StartTimer("GetSearchFiles");
			}
			var searchFiles = GetSearchFiles(searchDirs);
			if (Settings.DoTiming)
			{
				StopTimer("GetSearchFiles");
				if (Settings.PrintResults)
					PrintElapsed("GetSearchFiles");
			}
			if (Settings.Verbose)
			{
				Log(string.Format("\nFiles to be searched ({0}):", searchFiles.Count()));
				foreach (var f in searchFiles)
				{
					Log(FileUtil.GetRelativePath(f.FullName));
				}
				Log("");
			}

			if (Settings.DoTiming) {
				StartTimer("SearchFiles");
			}
			foreach (var f in searchFiles)
			{
				DoSearchFile(f);
			}
			if (Settings.DoTiming)
			{
				StopTimer("SearchFiles");
				if (Settings.PrintResults)
				{
					PrintElapsed("SearchFiles");
					PrintTotalElapsed();
				}
			}
		}

		public void DoSearchFile(SearchFile f)
		{
			if (f.Type == FileType.Text)
			{
				SearchTextFile(f);
			}
			else if (f.Type == FileType.Binary)
			{
				SearchBinaryFile(f);
			}
			else if (f.Type == FileType.Archive)
			{
				Log(string.Format("Skipping archive file {0}",
					FileUtil.GetRelativePath(f.FullName)));
			}
			else if (Settings.Verbose)
			{
				Log(string.Format("Skipping file {0}",
					FileUtil.GetRelativePath(f.FullName)));
			}
		}

		private void SearchTextFile(SearchFile f)
		{
			if (Settings.Verbose)
				Log(string.Format("Searching text file {0}",
					FileUtil.GetRelativePath(f.FullName)));
			if (Settings.MultiLineSearch)
				SearchTextFileContents(f);
			else
				SearchTextFileLines(f);
		}

		private void SearchTextFileContents(SearchFile f)
		{
			try
			{
				using (var sr = new StreamReader(f.FullName))
				{
					var contents = sr.ReadToEnd();
					var results = SearchContents(contents);
					foreach (SearchResult r in results)
					{
						r.File = f;
						AddSearchResult(r);
					}
				}
			}
			catch (IOException e)
			{
				Log(e.Message);
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
				var enumerableLines = EnumerableStringFromFile(f);
				var results = SearchLines(enumerableLines);

				foreach (var r in results)
				{
					r.File = f;
					AddSearchResult(r);
				}
			}
			catch (IOException e)
			{
				Log(e.Message);
			}
		}

		private static IEnumerable<string> EnumerableStringFromFile(SearchFile f)
		{
			using (var sr = new StreamReader(f.FullName))
			{
				// read each line, ensuring not null (EOF)
				string line;
				while ((line = sr.ReadLine()) != null)
				{
					// return trimmed line
					yield return line;
				}
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

		private IEnumerable<SearchResult> SearchLines(IEnumerable<string> lines)
		{
			var patternMatches = new Dictionary<Regex, int>();
			var results = new List<SearchResult>();
			var lineNum = 0;
			var linesBefore = new Queue<string>();
			var linesAfter = new Queue<string>();
			var lineEnumerator = lines.GetEnumerator();

			while (lineEnumerator.MoveNext() || linesAfter.Count > 0)
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
								continue;
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

		// TODO: switch to use SearchLines with buffering
		private void SearchBinaryFile(SearchFile f)
		{
			if (Settings.Verbose)
				Log(string.Format("Searching binary file {0}",
					FileUtil.GetRelativePath(f.FullName)));
			try
			{
				using (var sr = new StreamReader(f.FullName))
				{
					var contents = sr.ReadToEnd();
					foreach (var p in Settings.SearchPatterns.Where(p => p.Match(contents).Success)) {
						AddSearchResult(new SearchResult(p, f, 0, 0, 0, null));
					}
				}
			}
			catch (IOException e)
			{
				Log(e.Message);
			}
		}

		private void AddSearchResult(SearchResult searchResult)
		{
			Results.Add(searchResult);
		}

		public void PrintResults()
		{
			Log(string.Format("Search results ({0}):", Results.Count));
			foreach (var searchResult in Results)
			{
				Log(searchResult.ToString());
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
			Log(string.Format("\nDirectories with matches ({0}):", matchingDirs.Count()));
			foreach (var d in matchingDirs)
			{
				Log(FileUtil.GetRelativePath(d.FullName));
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
			Log(string.Format("\nFiles with matches ({0}):", matchingFiles.Count()));
			foreach (var f in matchingFiles)
			{
				Log(FileUtil.GetRelativePath(f.FullName));
			}
		}

		public IEnumerable<string> GetMatchingLines()
		{
			var lines = Results.Select(r => r.Line.Trim()).ToList();
			if (Settings.UniqueLines)
			{
				lines = new HashSet<string>(lines).ToList();
			}
			lines.Sort();
			return lines;
		}

		public void PrintMatchingLines()
		{
			var matchingLines = GetMatchingLines();
			var hdrText = Settings.UniqueLines ? "Unique lines with matches" : "Lines with matches";
			Log(string.Format("\n{0} ({1}):", hdrText, matchingLines.Count()));
			foreach (var m in matchingLines)
			{
				Log(m);
			}
		}
	}
}
