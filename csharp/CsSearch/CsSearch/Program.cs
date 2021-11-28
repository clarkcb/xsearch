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
					Common.Log("settings: " + settings + "\n");
				}

				if (settings.PrintUsage)
				{
					options.Usage();
				}

				var searcher = new Searcher(settings);
				searcher.Search();

				if (settings.PrintResults)
				{
					Common.Log("");
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
				Common.Log($"\nERROR: {e.Message}");
				options.Usage(1);
			}
		}
	}
}
