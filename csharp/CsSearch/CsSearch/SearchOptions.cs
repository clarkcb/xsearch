using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CsSearch
{
	class SearchOptions
	{
		public static List<SearchArgOption> ArgOptions =
			new List<SearchArgOption>
				{
					//new SearchArgOption("b", "numlinesbefore",
					//    (s, settings) => settings.SetProperty("numlinesbefore", int(s)),
					//    "Number of lines to show before every matched line (default: 0)"),
					//new SearchArgOption("B", "numlinesafter",
					//    (s, settings) => settings.SetProperty("numlinesafter", int(s)),
					//    "Number of lines to show after every matched line (default: 0)"),
					new SearchArgOption("d", "dirname",
					                    (s, settings) => settings.AddInDirPattern(s),
					                    "Specify name pattern for directories to include in search"),
					new SearchArgOption("D", "dirfilter",
					                    (s, settings) => settings.AddOutDirPattern(s),
					                    "Specify name pattern for directories to exclude from search"),
					new SearchArgOption("f", "filename",
					                    (s, settings) => settings.AddInFilePattern(s),
					                    "Specify name pattern for files to include in search"),
					new SearchArgOption("F", "filefilter",
					                    (s, settings) => settings.AddOutFilePattern(s),
					                    "Specify name pattern for files to exclude from search"),
					//new SearchArgOption("", "linesafterfilter",
					//    (s, settings) => settings.linesafterfilters.append(s),
					//    "Specify pattern to filter the "lines-after" lines on (used with --numlinesafter)"),
					//new SearchArgOption("", "linesaftersearch",
					//    (s, settings) => settings.linesaftersearches.append(s),
					//    "Specify pattern to search the "lines-after" lines on (used with --numlinesafter)"),
					//new SearchArgOption("", "linesbeforefilter",
					//    (s, settings) => settings.linesbeforefilters.append(s),
					//    "Specify pattern to filter the "lines-before" lines on (used with --numlinesbefore)"),
					//new SearchArgOption("", "linesbeforesearch",
					//    (s, settings) => settings.linesbeforesearches.append(s),
					//    "Specify pattern to search the \"lines-before\" lines on (used with --numlinesbefore)"),
					new SearchArgOption("s", "search",
					                    (s, settings) => settings.AddSearchPattern(s),
					                    "Specify search pattern"),
					new SearchArgOption("x", "ext",
					                    (s, settings) => settings.AddInExtension(s),
					                    "Specify extension for files to include in search"),
					new SearchArgOption("X", "extfilter",
					                    (s, settings) => settings.AddOutExtension(s),
					                    "Specify extension for files to exclude from search")

				};
		public static List<SearchFlagOption> FlagOptions =
			new List<SearchFlagOption>
				{
					new SearchFlagOption("1", "firstmatch",
					                     settings => settings.FirstMatch = true,
					                     "Capture only the first match for a file+search combination"),
					new SearchFlagOption("a", "allmatches",
					                     settings => settings.FirstMatch = false,
					                     "Capture all matches*"),
					new SearchFlagOption("", "debug",
					                     settings => settings.Debug = true,
					                     "Set output mode to debug"),
					new SearchFlagOption("h", "help",
					                     settings => settings.PrintUsage = true,
					                     "Print this usage and exit"),
					new SearchFlagOption("", "listfiles",
					                     settings => settings.ListFiles = true,
					                     "Generate a list of the matching files after searching"),
					new SearchFlagOption("", "listlines",
					                     settings => settings.ListLines = true,
					                     "Generate a list of the matching lines after searching"),
					//new SearchFlagOption("m", "multilinesearch",
					//                     settings => settings.SetProperty("multilinesearch", true),
					//                     "Search files by line*"),
					new SearchFlagOption("p", "printmatches",
					                     settings => settings.PrintResults = true,
					                     "Print matches to stdout as found*"),
					new SearchFlagOption("P", "noprintmatches",
					                     settings => settings.PrintResults = false,
					                     "Suppress printing of matches to stdout"),
					new SearchFlagOption("t", "dotiming",
					                     settings => settings.DoTiming = true,
					                     "Time search execution"),
					new SearchFlagOption("v", "verbose",
					                     settings => settings.Verbose = true,
					                     "Specify verbose output"),
					new SearchFlagOption("V", "version",
					                     settings => settings.PrintVersion = true,
					                     "Print the version and exit"),
					new SearchFlagOption("z", "searchcompressed",
					                     settings => settings.SearchCompressed = true,
					                     "Search compressed files (bz2, gz, tar, zip)*"),
					new SearchFlagOption("Z", "nosearchcompressed",
					                     settings => settings.SearchCompressed = false,
					                     "Do not search compressed files (bz2, gz, tar, zip)"),
				};

		public List<SearchOption> Options { get; private set; }
		public Dictionary<string, SearchOption> ArgDictionary { get; private set; }
		public Dictionary<string, SearchOption> FlagDictionary { get; private set; }

		private static Dictionary<string, SearchOption> DictionaryFromOptions(IEnumerable<SearchOption> options)
		{
			var dict = new Dictionary<string, SearchOption>();
			try
			{
				foreach (var opt in options)
				{
					if (!string.IsNullOrWhiteSpace(opt.ShortArg))
					{
						dict.Add(opt.ShortArg, opt);
					}
					dict.Add(opt.LongArg, opt);
				}
			}
			catch (ArgumentException e)
			{
				Console.WriteLine("Exception: " + e.Message);
			}
			return dict;
		}

		public SearchOptions()
		{
			Options = new List<SearchOption>(ArgOptions);
			Options.AddRange(new List<SearchOption>(FlagOptions));
			ArgDictionary = DictionaryFromOptions(ArgOptions);
			FlagDictionary = DictionaryFromOptions(FlagOptions);
		}

		public SearchSettings SettingsFromArgs(IEnumerable<string> args)
		{
			var settings = new SearchSettings();
			var queue = new Queue<string>(args);

			while (queue.Count > 0)
			{
				var s = queue.Dequeue();
				if (s.StartsWith("-"))
				{
					try
					{
						while (s.StartsWith("-"))
						{
							s = s.Substring(1);
						}
					}
					catch (InvalidOperationException e)
					{
						throw new SearchArgumentException(e.Message);
					}
					if (string.IsNullOrWhiteSpace(s))
					{
						throw new SearchArgumentException("Invalid option encountered");
					}
					if (ArgDictionary.ContainsKey(s))
					{
						try
						{
							((SearchArgOption)ArgDictionary[s]).Action(queue.Dequeue(), settings);
						}
						catch (InvalidOperationException e)
						{
							throw new SearchArgumentException(e.Message);
						}
					}
					else if (FlagDictionary.ContainsKey(s))
					{
						try
						{
							((SearchFlagOption)FlagDictionary[s]).Action(settings);
						}
						catch (InvalidOperationException e)
						{
							throw new SearchArgumentException(e.Message);
						}
					}
					else
					{
						throw new SearchArgumentException("Unknown option: " + s);
					}
				}
				else
				{
					settings.StartPath = s;
				}
			}
			if (settings.StartPath == null)
			{
				throw new SearchArgumentException("Missing startpath");
			}
			if (settings.SearchPatterns.Count < 1)
			{
				throw new SearchArgumentException("No search patterns specified");
			}
			return settings;
		}

		public void Usage(int exitCode = 0)
		{
			Console.WriteLine(GetUsageString());
			Environment.Exit(exitCode);
		}

		public string GetUsageString()
		{
			var sb = new StringBuilder();
			sb.AppendLine("\nUsage:");
			sb.AppendLine(" CsSearch.exe [options] <startpath>\n");
			sb.AppendLine("Options:");
			var optStrings = new List<string>();
			var optDescs = new List<string>();
			var longest = 0;
			foreach (var opt in Options.OrderBy(o => o.SortArg))
			{
				var optString = new StringBuilder();
				if (!string.IsNullOrWhiteSpace(opt.ShortArg))
				{
					optString.Append("-" + opt.ShortArg + ",");
				}
				optString.Append("--" + opt.LongArg);
				if (optString.Length > longest)
				{
					longest = optString.Length;
				}
				optStrings.Add(optString.ToString());
				optDescs.Add(opt.Description);
			}
			var format = " {0,-"+longest+"}  {1}";
			for (var i = 0; i < optStrings.Count; i++)
			{
				sb.AppendLine(string.Format(format, optStrings[i], optDescs[i]));
			}
			return sb.ToString();
		}
	}
}
