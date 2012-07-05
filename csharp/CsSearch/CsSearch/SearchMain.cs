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
				Console.WriteLine("Matches found: " + searcher.Results.Count);

				if (settings.ListFiles) {
					Console.WriteLine("\nFiles with matches:");
					foreach (var f in searcher.FileSet) {
						Console.WriteLine(f.FullName);
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
