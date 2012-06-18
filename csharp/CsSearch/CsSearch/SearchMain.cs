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
				if (settings.SearchPatterns.Count < 1)
					Usage(options);
				var searcher = new Searcher(settings);
				searcher.Search();
				Console.WriteLine("Matches found: " + searcher.Results.Count);

				if (settings.ListFiles) {
					Console.WriteLine("\nFiles with matches:");
					foreach (var f in searcher.FileSet) {
						Console.WriteLine(f.FullName);
					}
				}
			}
			catch (SearchArgumentException)
			{
				Usage(options);
			}
		}
		static void Usage(SearchOptions options)
		{
			Console.WriteLine(options.GetUsageString());
			Environment.Exit(1);
		}
	}
}
