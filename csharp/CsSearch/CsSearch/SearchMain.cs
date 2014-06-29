using System;

namespace CsSearch
{
	class SearchMain
	{
		static void Main(string[] args)
		{
			var options = new SearchOptions();
			SearchSettings settings;
			try
			{
				settings = options.SettingsFromArgs(args);
				var searcher = new Searcher(settings);
				searcher.Search();

				if (settings.PrintResults)
				{
					Console.WriteLine();
					searcher.PrintResults();
				}

				if (settings.ListDirs)
				{
					Console.WriteLine(string.Format("\nDirectories with matches ({0}):", searcher.DirNameSet.Count));
					foreach (var d in searcher.DirNameSet)
					{
						Console.WriteLine(d);
					}
				}

				if (settings.ListFiles)
				{
					Console.WriteLine(string.Format("\nFiles with matches ({0}):", searcher.FileNameSet.Count));
					foreach (var f in searcher.FileNameSet)
					{
						Console.WriteLine(f);
					}
				}
			}
			catch (SearchArgumentException e)
			{
				Console.WriteLine("Error: " + e.Message);
				options.Usage(1);
			}
		}
	}
}
