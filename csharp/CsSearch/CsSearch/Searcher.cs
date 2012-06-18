using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace CsSearch
{
	public class Searcher
	{
		private List<Func<DirectoryInfo, bool>> _dirFilterPredicates;
		private List<Func<FileInfo, bool>> _fileFilterPredicates;
		public SearchSettings Settings { get; private set; }
		public IList<SearchResult> Results { get; private set; }
		public ISet<FileInfo> FileSet { get; private set; }
		public IDictionary<string,Stopwatch> Timers { get; private set; }

		protected List<Func<DirectoryInfo, bool>> DirectoryFilterPredicates
		{
			get
			{
				if (_dirFilterPredicates == null)
				{
					_dirFilterPredicates = new List<Func<DirectoryInfo, bool>>();
					if (Settings.InDirPatterns.Count > 0)
					{
						_dirFilterPredicates.Add(d => Settings.InDirPatterns.Any(p => p.Match(d.FullName).Success));
					}
					if (Settings.OutDirPatterns.Count > 0)
					{
						_dirFilterPredicates.Add(d => !Settings.OutDirPatterns.Any(p => p.Match(d.FullName).Success));
					}
				}
				return _dirFilterPredicates;
			}
		}

		protected List<Func<FileInfo, bool>> FileFilterPredicates
		{
			get
			{
				if (_fileFilterPredicates == null)
				{
					_fileFilterPredicates = new List<Func<FileInfo, bool>>();

					if (Settings.InExtensions.Count > 0)
					{
						_fileFilterPredicates.Add(f => Settings.InExtensions.Contains(f.Extension));
					}
					if (Settings.OutExtensions.Count > 0)
					{
						_fileFilterPredicates.Add(f => !Settings.OutExtensions.Contains(f.Extension));
					}
					{
						_fileFilterPredicates.Add(f => Settings.InExtensions.Contains(f.Extension));
					}
					if (Settings.InFilePatterns.Count > 0)
					{
						_fileFilterPredicates.Add(f => Settings.InFilePatterns.Any(p => p.Match(f.Name).Success));
					}
					if (Settings.OutFilePatterns.Count > 0)
					{
						_fileFilterPredicates.Add(f => !Settings.OutFilePatterns.Any(p => p.Match(f.Name).Success));
					}
				}
				return _fileFilterPredicates;
			}
		}

		public Searcher(SearchSettings settings)
		{
			Settings = settings;
			if (Settings.Verbose)
				Console.WriteLine(Settings + "\n");
			Results = new List<SearchResult>();
			FileSet = new HashSet<FileInfo>();
			Timers = new Dictionary<string,Stopwatch>();
		}

		private bool IsTargetDirectory(DirectoryInfo d)
		{
			return DirectoryFilterPredicates.Count == 0 || DirectoryFilterPredicates.All(p => p(d));
		}

		private bool IsTargetFile(FileInfo f)
		{
			return FileFilterPredicates.Count == 0  || FileFilterPredicates.All(p => p(f));
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

		public IEnumerable<FileInfo> GetSearchFiles(DirectoryInfo dir)
		{
			IEnumerable<FileInfo> files = new List<FileInfo>();
			try
			{
				files = dir.EnumerateFiles().Where(IsTargetFile);
				var dirs = dir.EnumerateDirectories().Where(IsTargetDirectory);
				return dirs.Aggregate(files, (current, d) => current.Concat(GetSearchFiles(d)));
			}
			catch (IOException e)
			{
				if (Settings.Verbose)
					Console.WriteLine(String.Format("Error while accessing dir {0}: {1}", dir.FullName, e.Message));
			}
			return files;
		}

		public void Search()
		{
			var startDir = new DirectoryInfo(Settings.StartPath);
			if (Settings.Verbose)
				Console.WriteLine("Starting directory: " + startDir.FullName + "\n");
			if (!startDir.Exists)
			{
				throw new FileNotFoundException("File not found", startDir.FullName);
			}
			if (!IsTargetDirectory(startDir))
			{
				throw new Exception("Starting directory matches an exclusion filter or does not match an inclusion filter");
			}
			if (Settings.DoTiming)
			{
				StartTimer("GetSearchFiles");
			}
			var files = GetSearchFiles(startDir);
			if (Settings.DoTiming)
			{
				StopTimer("GetSearchFiles");
			}
			if (Settings.Verbose || Settings.Debug)
			{
				Console.WriteLine("Files to be searched:");
				foreach (var f in files)
				{
					Console.WriteLine(f.FullName);
				}
				Console.WriteLine();
			}

			if (Settings.DoTiming) {
				StartTimer("SearchFiles");
			}
			foreach (var f in files)
			{
				SearchFile(f);
			}
			if (Settings.DoTiming) {
				StopTimer("SearchFiles");
			}
		}

		public bool IsSearchableFile(FileInfo f)
		{
			return !Settings.NosearchExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		private bool IsBinaryFile(FileInfo f)
		{
			return Settings.BinaryExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		private bool IsTextFile(FileInfo f)
		{
			return Settings.TextExtensions.Contains(f.Extension.ToLowerInvariant());
		}

		private bool IsUnknownFile(FileInfo f)
		{
			return (Settings.UnknownExtensions.Contains(f.Extension.ToLowerInvariant()) ||
					(!Settings.TextExtensions.Contains(f.Extension.ToLowerInvariant()) &&
					 !Settings.BinaryExtensions.Contains(f.Extension.ToLowerInvariant()) &&
					 !Settings.UnknownExtensions.Contains(f.Extension.ToLowerInvariant())));
		}

		public void SearchFile(FileInfo f)
		{
			if (IsUnknownFile(f))
			{
				Console.WriteLine("Skipping file of unknown type: " + f.FullName);
			}
			else if (IsSearchableFile(f))
			{
				//if (Settings.DoTiming) {
				//    StartTimer(f.FullName);
				//}
				if (IsTextFile(f))
				{
					SearchTextFile(f);
				}
				else if (IsBinaryFile(f))
				{
					SearchBinaryFile(f);
				}
				//if (Settings.DoTiming) {
				//    StopTimer(f.FullName);
				//}
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
			try
			{
				using (var sr = new StreamReader(f.FullName))
				{
					var lineNum = 0;
					String line;
					while ((line = sr.ReadLine()) != null)
					{
						lineNum++;
						foreach (var p in Settings.SearchPatterns.Where(p => p.Match(line).Success))
						{
							AddSearchResult(new SearchResult(p, f, lineNum, line));
						}
					}
				}
			}
			catch (IOException e)
			{
				Console.WriteLine(e.Message);
			}
		}

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
						AddSearchResult(new SearchResult(p, f, 0, null));
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
			Console.WriteLine(searchResult);
			Results.Add(searchResult);
			FileSet.Add(searchResult.File);
		}
	}
}
