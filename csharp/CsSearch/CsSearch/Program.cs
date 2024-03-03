using CsFindLib;
using CsSearchLib;

namespace CsSearch
{
	static class Program
	{
		static void Main(string[] args)
		{
			var options = new SearchOptions();
			try
			{
				var settings = options.SettingsFromArgs(args);

				if (settings.Debug)
				{
					Logger.Log("settings: " + settings + "\n");
				}

				if (settings.PrintUsage)
				{
					options.Usage();
				}

				var searcher = new Searcher(settings);
				searcher.Search();

				if (settings.PrintResults)
				{
					Logger.Log("");
					searcher.PrintResults();
				}

				if (settings.PrintDirs)
				{
					searcher.PrintMatchingDirs();
				}

				if (settings.PrintFiles)
				{
					searcher.PrintMatchingFiles();
				}

				if (settings.PrintLines)
				{
					searcher.PrintMatchingLines();
				}
			}
			catch (SearchException e)
			{
				Logger.LogError(e.Message);
				options.Usage(1);
			}
		}
	}
}
