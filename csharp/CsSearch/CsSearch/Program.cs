using CsFindLib;
using CsSearchLib;

namespace CsSearch
{
	static class Program
	{
		public static void Main(string[] args)
		{
			var colorize = true;
			SearchOptions? options = null;
			try
			{
				options = new SearchOptions();
				var settings = options.SettingsFromArgs(args);
				colorize = settings.Colorize;

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

				if (settings.PrintMatches)
				{
					searcher.PrintMatches(results, formatter);
				}
			}
			catch (SearchException e)
			{
				Logger.Log("");
				Logger.LogError(e.Message, colorize);
				options?.Usage(1);
			}
		}
	}
}
