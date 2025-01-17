﻿using CsFindLib;
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

				if (settings.PrintResults)
				{
					Logger.Log("");
					searcher.PrintResults(results);
				}

				if (settings.PrintDirs)
				{
					searcher.PrintMatchingDirs(results);
				}

				if (settings.PrintFiles)
				{
					searcher.PrintMatchingFiles(results);
				}

				if (settings.PrintLines)
				{
					searcher.PrintMatchingLines(results);
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
