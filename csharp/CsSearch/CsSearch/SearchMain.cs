using System;

namespace CsSearch
{
	class SearchMain
	{
		static void Main(string[] args)
		{
			SearchSettings settings;
			try
			{
				settings = Arguments.SettingsFromArgs(args);
			}
			catch (SearchArgumentException)
			{
				Usage(1);
				return;
			}
			if (settings.SearchPatterns.Count < 1)
				Usage(1);
			var searcher = new Searcher(settings);
			searcher.Search();
			Console.WriteLine("Matches found: " + searcher.Results.Count);

			if (settings.ListFiles)
			{
				Console.WriteLine("\nFiles with matches:");
				foreach (var f in searcher.FileSet)
				{
					Console.WriteLine(f.FullName);
				}
			}
		}

		static void Usage(int errCode = 0)
		{
			Console.WriteLine("\nUsage:\n");
			Console.WriteLine("CsSearch.exe [options] -s \"<searchpattern>\" <startdir>\n");
			Console.WriteLine("Options:");
			Console.WriteLine("  -d \"<dirname_pattern>\"     Pattern for dirnames to include");
			Console.WriteLine("  -D \"<dirname_pattern>\"     Pattern for dirnames to exclude");
			Console.WriteLine("  -f \"<filename_pattern>\"    Pattern for filenames to include");
			Console.WriteLine("  -F \"<filename_pattern>\"    Pattern for filenames to exclude");
			Console.WriteLine("  --filelist                 Print list of matching files at end");
			Console.WriteLine("  -t                         Time the execution");
			Console.WriteLine("  -v                         Set verbose mode");
			Console.WriteLine("  -x \"<ext1>[,<ext2>]\"       Extension(s) for files to include");
			Console.WriteLine("  -X \"<ext1>[,<ext2>]\"       Extension(s) for files to exclude");
			Console.WriteLine();
			Environment.Exit(errCode);
		}
	}
}
