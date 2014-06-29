using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace CsSearch
{
	public class Searcher
	{
		private readonly FileUtil _fileUtil;
		public SearchSettings Settings { get; private set; }
		public IList<SearchResult> Results { get; private set; }
		public ISet<string> DirNameSet { get; private set; }
		public ISet<string> FileNameSet { get; private set; }
		public IDictionary<string, Stopwatch> Timers { get; private set; }

		public Searcher(SearchSettings settings)
		{
			Settings = settings;
			if (Settings.Verbose)
				Console.WriteLine(Settings + "\n");
			_fileUtil = new FileUtil();
			Results = new List<SearchResult>();
			DirNameSet = new HashSet<string>();
			FileNameSet = new HashSet<string>();
			Timers = new Dictionary<string, Stopwatch>();
		}

		private bool IsSearchDirectory(DirectoryInfo d)
		{
			if (Settings.InDirPatterns.Count > 0 && !Settings.InDirPatterns.Any(p => p.Match(d.FullName).Success))
				return false;
			if (Settings.OutDirPatterns.Count > 0 && Settings.OutDirPatterns.Any(p => p.Match(d.FullName).Success))
				return false;
			return true;
		}

		private bool IsSearchFile(FileInfo f)
		{
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

		public IEnumerable<DirectoryInfo> GetSearchDirs(DirectoryInfo dir)
		{
			IEnumerable<DirectoryInfo> dirs = new List<DirectoryInfo>();
			try
			{
				dirs = dir.EnumerateDirectories().Where(IsSearchDirectory);
				return dirs.Aggregate(dirs, (current, d) => current.Concat(GetSearchDirs(d)));
			}
			catch (IOException e)
			{
				if (Settings.Verbose)
					Console.WriteLine(String.Format("Error while accessing dir {0}: {1}", dir.FullName, e.Message));
			}
			return dirs;
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
				dirSearchFiles = dir.EnumerateFiles().Where(IsSearchFile);
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
			var startDir = new DirectoryInfo(Settings.StartPath);
			var searchDirs = new List<DirectoryInfo>();
			searchDirs.Add(startDir);
			if (Settings.DoTiming)
			{
				StartTimer("GetSearchDirs");
			}
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
				//if (Settings.DoTiming) {
				//    StartTimer(f.FullName);
				//}
				if (_fileUtil.IsTextFile(f))
				{
					SearchTextFile(f);
				}
				else if (_fileUtil.IsBinaryFile(f))
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
			Results.Add(searchResult);
			DirNameSet.Add(searchResult.File.Directory.ToString());
			FileNameSet.Add(searchResult.File.ToString());
		}

		public void PrintResults()
		{
			Console.WriteLine(string.Format("Search results ({0}):", Results.Count));
			foreach (var searchResult in Results)
			{
				Console.WriteLine(searchResult);
			}
		}
	}
}
