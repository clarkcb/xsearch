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
				var results = searcher.Search();
				var formatter = new SearchResultFormatter(settings);

				if (settings.PrintResults)
				{
					Logger.Log("");
					Searcher.PrintResults(results, formatter);
				}

				if (settings.PrintDirs)
				{
					Searcher.PrintMatchingDirs(results, formatter);
				}

				if (settings.PrintFiles)
				{
					Searcher.PrintMatchingFiles(results, formatter);
				}

				if (settings.PrintLines)
				{
					searcher.PrintMatchingLines(results, formatter);
				}
			}
			catch (SearchException e)
			{
				Logger.Log("");
				Logger.LogError(e.Message);
				options.Usage(1);
			}
		}
	}
}
