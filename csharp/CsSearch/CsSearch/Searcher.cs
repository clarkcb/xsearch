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
		private readonly FileUtil _fileUtil;
		public SearchSettings Settings { get; private set; }
		public IList<SearchResult> Results { get; private set; }
		public ISet<DirectoryInfo> DirSet { get; private set; }
		public ISet<FileInfo> FileSet { get; private set; }
		public IDictionary<string, Stopwatch> Timers { get; private set; }

		public Searcher(SearchSettings settings)
		{
			Settings = settings;
			if (Settings.Verbose)
				Console.WriteLine(Settings + "\n");
			ValidateSettings();
			_fileUtil = new FileUtil();
			Results = new List<SearchResult>();
			DirSet = new HashSet<DirectoryInfo>();
			FileSet = new HashSet<FileInfo>();
			Timers = new Dictionary<string, Stopwatch>();
		}

		private void ValidateSettings()
		{
			if (string.IsNullOrEmpty(Settings.StartPath))
				throw new SearchArgumentException("Startpath not defined");
			if (!(new DirectoryInfo(Settings.StartPath)).Exists)
				throw new SearchArgumentException("Startpath not found");
			if (Settings.SearchPatterns.Count < 1)
				throw new SearchArgumentException("No search patterns specified");
		}

		private bool IsHiddenFile(FileSystemInfo f)
		{
			return (f.Attributes & FileAttributes.Hidden) != 0;
		}

		private bool IsSearchDirectory(DirectoryInfo d)
		{
			if (Settings.ExcludeHidden && IsHiddenFile(d))
				return false;
			if (Settings.InDirPatterns.Count > 0 && !Settings.InDirPatterns.Any(p => p.Match(d.Name).Success))
				return false;
			if (Settings.OutDirPatterns.Count > 0 && Settings.OutDirPatterns.Any(p => p.Match(d.Name).Success))
				return false;
			return true;
		}

		private bool IsSearchFile(FileInfo f)
		{
			if (Settings.ExcludeHidden && IsHiddenFile(f))
				return false;
			if (Settings.InExtensions.Count > 0 && !Settings.InExtensions.Contains(f.Extension))
				return false;
			if (Settings.OutExtensions.Count > 0 && Settings.OutExtensions.Contains(f.Extension))
				return false;
			if (Settings.InFilePatterns.Count > 0 && !Settings.InFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			if (Settings.OutFilePatterns.Count > 0 && Settings.OutFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			return true;
		}

		private bool IsArchiveSearchFile(FileInfo f)
		{
			if (Settings.ExcludeHidden && IsHiddenFile(f))
				return false;
			if (Settings.InArchiveFilePatterns.Count > 0 && !Settings.InArchiveFilePatterns.Any(p => p.Match(f.Name).Success))
				return false;
			if (Settings.OutArchiveFilePatterns.Count > 0 && Settings.OutArchiveFilePatterns.Any(p => p.Match(f.Name).Success))
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
			PrintElapsed(name, timer.Elapsed);
		}

		public void PrintElapsed (string name, TimeSpan ts)
		{
			var elapsedTime =
				String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
							  ts.Hours, ts.Minutes, ts.Seconds,
							  ts.Milliseconds/10);
			Console.WriteLine(string.Format("Elapsed time for {0}: {1}", name, elapsedTime));
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

		public IEnumerable<DirectoryInfo> RecGetSearchDirs(DirectoryInfo dir)
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
					Console.WriteLine(String.Format("Error while accessing dir {0}: {1}", dir.FullName, e.Message));
			}
			return searchDirs;
		}
		private bool IsValidSearchFile(FileInfo f)
		{
			return
				(_fileUtil.IsArchiveFile(f) && Settings.SearchArchives && IsArchiveSearchFile(f))
				||
				(!Settings.ArchivesOnly && IsSearchFile(f));
		}

		public IEnumerable<FileInfo> GetSearchFilesForDir(DirectoryInfo dir)
		{
			if (Settings.Debug)
			{
				Console.WriteLine("Getting search files under "+dir);
			}
			IEnumerable<FileInfo> dirSearchFiles = new List<FileInfo>();
			try
			{
				dirSearchFiles = dir.EnumerateFiles().Where(IsValidSearchFile);
			}
			catch (IOException e)
			{
				if (Settings.Verbose)
					Console.WriteLine(String.Format("Error while accessing dir {0}: {1}", dir.FullName, e.Message));
			}
			return dirSearchFiles;
		}

		public IEnumerable<FileInfo> GetSearchFiles(IEnumerable<DirectoryInfo> dirs)
		{
			var searchFiles = new List<FileInfo>();
			foreach (var d in dirs)
			{
				searchFiles.AddRange(GetSearchFilesForDir(d));
			}
			return searchFiles;
		}

		public void Search()
		{
			if (Settings.DoTiming)
			{
				StartTimer("GetSearchDirs");
			}
			var startDir = new DirectoryInfo(Settings.StartPath);
			var searchDirs = new List<DirectoryInfo>();
			searchDirs.AddRange(GetSearchDirs(startDir));
			if (Settings.DoTiming)
			{
				StopTimer("GetSearchDirs");
			}
			if (Settings.Verbose)
			{
				Console.WriteLine(string.Format("Directories to be searched ({0}):", searchDirs.Count));
				foreach (var d in searchDirs)
				{
					Console.WriteLine(d.FullName);
				}
				Console.WriteLine();
			}

			if (Settings.DoTiming)
			{
				StartTimer("GetSearchFiles");
			}
			var searchFiles = GetSearchFiles(searchDirs);
			if (Settings.DoTiming)
			{
				StopTimer("GetSearchFiles");
			}
			if (Settings.Verbose)
			{
				Console.WriteLine(string.Format("\nFiles to be searched ({0}):", searchFiles.Count()));
				foreach (var f in searchFiles)
				{
					Console.WriteLine(f.FullName);
				}
				Console.WriteLine();
			}

			if (Settings.DoTiming) {
				StartTimer("SearchFiles");
			}
			foreach (var f in searchFiles)
			{
				SearchFile(f);
			}
			if (Settings.DoTiming)
			{
				StopTimer("SearchFiles");
			}
		}

		public void SearchFile(FileInfo f)
		{
			if (_fileUtil.IsUnknownFile(f))
			{
				Console.WriteLine("Skipping file of unknown type: " + f.FullName);
			}
			else if (_fileUtil.IsSearchableFile(f))
			{
				if (_fileUtil.IsTextFile(f))
				{
					SearchTextFile(f);
				}
				else if (_fileUtil.IsBinaryFile(f))
				{
					SearchBinaryFile(f);
				}
			}
			else if (Settings.Verbose)
			{
				Console.WriteLine("Skipping unsearchable file: " + f.FullName);
			}
		}

		private void SearchTextFile(FileInfo f)
		{
			if (Settings.Verbose)
				Console.WriteLine("Searching text file " + f.FullName);
			// TODO: SearchTextFileContents
			if (Settings.MultiLineSearch)
				SearchTextFileContents(f);
			else
				SearchTextFileLines(f);
		}

		private void SearchTextFileContents(FileInfo f)
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
				Console.WriteLine(e.Message);
			}
		}

		private int CountNewlines(string text)
		{
			return text.Count(c => c == '\n');
		}

		private int GetStartLineIndex(string text, int matchIndex)
		{
			var startIndex = matchIndex;
			while (startIndex > 0 && text[startIndex-1] != '\n')
				startIndex--;
			return startIndex;
		}

		private int GetEndLineIndex(string text, int matchIndex)
		{
			var endIndex = matchIndex;
			while (endIndex < text.Length && text[endIndex] != '\r' &&
				text[endIndex] != '\n')
				endIndex++;
			return endIndex;
		}

		private IEnumerable<SearchResult> SearchContents(string contents)
		{
			var patternMatches = new Dictionary<Regex, int>();
			var results = new List<SearchResult>();

			foreach (var p in Settings.SearchPatterns)
			{
				var match = p.Match(contents);
				while (match.Success)
				{
					//TODO: add lineNum and line retrieval from contents
					var lineNum = CountNewlines(contents.Substring(0, match.Index)) + 1;
					var startIndex = GetStartLineIndex(contents, match.Index);
					var endIndex = GetEndLineIndex(contents, match.Index);
					var line = contents.Substring(startIndex, endIndex - startIndex);
					results.Add(new SearchResult(
						p,
						null,
						lineNum,
						match.Index - startIndex,
						match.Index - startIndex + match.Length,
						line,
						new List<string>(),
						new List<string>()));
					patternMatches[p] = 1;

					if (Settings.FirstMatch && patternMatches.ContainsKey(p))
					{
						break;
					}
					match = match.NextMatch();
				}
			}
			return results;
		}

		private void SearchTextFileLines(FileInfo f)
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
				Console.WriteLine(e.Message);
			}
		}

		private static IEnumerable<string> EnumerableStringFromFile(FileInfo f)
		{
			string line;
			//using (var file = System.IO.File.OpenText(fileName))
			using (var sr = new StreamReader(f.FullName))
			{
				// read each line, ensuring not null (EOF)
				while ((line = sr.ReadLine()) != null)
				{
					// return trimmed line
					yield return line;
				}
			}
		}

		private IEnumerable<SearchResult> SearchLines(IEnumerable<string> lines)
		{
			var patternMatches = new Dictionary<Regex, int>();
			var results = new List<SearchResult>();
			var lineNum = 0;
			foreach (var line in lines)
			{
				lineNum++;
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
							match.Index,
							match.Index + match.Length,
							line,
							new List<string>(),
							new List<string>()));
						patternMatches[p] = 1;
					}
				}
			}
			return results;
		}

		// TODO: switch to use SearchLines with buffering
		private void SearchBinaryFile(FileInfo f)
		{
			if (Settings.Verbose)
				Console.WriteLine("Searching binary file " + f.FullName);
			try
			{
				using (var sr = new StreamReader(f.FullName))
				{
					var contents = sr.ReadToEnd();
					foreach (var p in Settings.SearchPatterns.Where(p => p.Match(contents).Success)) {
						AddSearchResult(new SearchResult(p, f, 0, 0, 0, null,
							new List<string>(), new List<string>()));
					}
				}
			}
			catch (IOException e)
			{
				Console.WriteLine(e.Message);
			}
		}

		private void AddSearchResult(SearchResult searchResult)
		{
			Results.Add(searchResult);
			DirSet.Add(searchResult.File.Directory);
			FileSet.Add(searchResult.File);
		}

		public void PrintResults()
		{
			Console.WriteLine(string.Format("Search results ({0}):", Results.Count));
			foreach (var searchResult in Results)
			{
				Console.WriteLine(searchResult);
			}
		}

		public IEnumerable<DirectoryInfo> GetMatchingDirs()
		{
			IEnumerable<DirectoryInfo> matchingDirs = new List<DirectoryInfo>(DirSet.Distinct()).OrderBy(d => d.FullName);
			return matchingDirs;
		}

		public void PrintMatchingDirs()
		{
			IEnumerable<DirectoryInfo> matchingDirs = GetMatchingDirs();
			Console.WriteLine(string.Format("\nDirectories with matches ({0}):", matchingDirs.Count()));
			foreach (var d in matchingDirs)
			{
				Console.WriteLine(d);
			}
		}

		public IEnumerable<FileInfo> GetMatchingFiles()
		{
			IEnumerable<FileInfo> matchingFiles = new List<FileInfo>(FileSet).OrderBy(d => d.FullName);
			return matchingFiles;
		}

		public void PrintMatchingFiles()
		{
			IEnumerable<FileInfo> matchingFiles = GetMatchingFiles();
			Console.WriteLine(string.Format("\nFiles with matches ({0}):", matchingFiles.Count()));
			foreach (var f in matchingFiles)
			{
				Console.WriteLine(f);
			}
		}

		public IEnumerable<string> GetMatchingLines()
		{
			List<string> matchingLines = new List<string>();
			foreach (var r in Results)
			{
				matchingLines.Add(r.Line.Trim());
			}
			return matchingLines;
		}

		public void PrintMatchingLines()
		{
			IEnumerable<string> matchingLines = GetMatchingLines();
			Console.WriteLine(string.Format("\nLines with matches ({0}):", matchingLines.Count()));
			foreach (var m in matchingLines)
			{
				Console.WriteLine(m);
			}
		}
	}
}
