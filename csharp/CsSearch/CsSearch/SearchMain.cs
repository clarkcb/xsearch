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
					searcher.PrintMatchingDirs();
				}

				if (settings.ListFiles)
				{
					searcher.PrintMatchingFiles();
				}

				if (settings.ListLines)
				{
					searcher.PrintMatchingLines();
				}
			}
			catch (SearchException e)
			{
				Console.WriteLine("Error: {0}", e.Message);
				options.Usage(1);
			}
		}
	}
}
