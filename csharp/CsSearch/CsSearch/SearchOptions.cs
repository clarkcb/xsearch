using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace CsSearch
{
	public class SearchOptions
	{
		private readonly string _searchOptionsResource;

		public static Dictionary<string, Action<string, SearchSettings>> ArgActionDictionary =
			new Dictionary<string,Action<string,SearchSettings>>
				{
					{ "in-archiveext", (s, settings) => settings.AddInArchiveExtension(s) },
					{ "in-archivefilepattern", (s, settings) => settings.AddInArchiveFilePattern(s) },
					{ "in-dirpattern", (s, settings) => settings.AddInDirPattern(s) },
					{ "in-ext", (s, settings) => settings.AddInExtension(s) },
					{ "in-filepattern", (s, settings) => settings.AddInFilePattern(s) },
					{ "in-linesafterpattern", (s, settings) => settings.AddInLinesAfterPattern(s) },
					{ "in-linesbeforepattern", (s, settings) => settings.AddInLinesBeforePattern(s) },
					{ "linesafter", (s, settings) => settings.LinesAfter = int.Parse(s) },
					{ "linesaftertopattern", (s, settings) => settings.AddLinesAfterToPattern(s) },
					{ "linesafteruntilpattern", (s, settings) => settings.AddLinesAfterUntilPattern(s) },
					{ "linesbefore", (s, settings) => settings.LinesBefore = int.Parse(s) },
					{ "maxlinelength", (s, settings) => settings.MaxLineLength = int.Parse(s) },
					{ "out-archiveext", (s, settings) => settings.AddOutArchiveExtension(s) },
					{ "out-archivefilepattern", (s, settings) => settings.AddOutArchiveFilePattern(s) },
					{ "out-dirpattern", (s, settings) => settings.AddOutDirPattern(s) },
					{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
					{ "out-filepattern", (s, settings) => settings.AddOutFilePattern(s) },
					{ "out-linesafterpattern", (s, settings) => settings.AddOutLinesAfterPattern(s) },
					{ "out-linesbeforepattern", (s, settings) => settings.AddOutLinesBeforePattern(s) },
					{ "search", (s, settings) => settings.AddSearchPattern(s) },
				};

		public static Dictionary<string, Action<SearchSettings>> FlagActionDictionary =
			new Dictionary<string,Action<SearchSettings>>
				{
					{ "allmatches", settings => settings.FirstMatch = false },
					{ "archivesonly", settings => settings.SetArchivesOnly() },
					{ "debug", settings => settings.SetDebug() },
					{ "dotiming", settings => settings.DoTiming = true },
					{ "excludehidden", settings => settings.ExcludeHidden = true },
					{ "firstmatch", settings => settings.FirstMatch = true },
					{ "help", settings => settings.PrintUsage = true },
					{ "includehidden", settings => settings.ExcludeHidden = false },
					{ "listdirs", settings => settings.ListDirs = true },
					{ "listfiles", settings => settings.ListFiles = true },
					{ "listlines", settings => settings.ListLines = true },
					{ "multilinesearch", settings => settings.MultiLineSearch = true },
					{ "noprintmatches", settings => settings.PrintResults = false },
					{ "norecursive", settings => settings.Recursive = false },
					{ "nosearcharchives", settings => settings.SearchArchives = false },
					{ "printmatches", settings => settings.PrintResults = true },
					{ "recursive", settings => settings.Recursive = true },
					{ "searcharchives", settings => settings.SearchArchives = true },
					{ "uniquelines", settings => settings.UniqueLines = true },
					{ "verbose", settings => settings.Verbose = true },
					{ "version", settings => settings.PrintVersion = true },
				};

		public List<SearchOption> Options { get; private set; }
		public Dictionary<string, SearchOption> ArgDictionary { get; private set; }
		public Dictionary<string, SearchOption> FlagDictionary { get; private set; }

		private void SetOptionsFromXml()
		{
			var doc = XDocument.Parse(_searchOptionsResource);
			foreach (var f in doc.Descendants("searchoption"))
			{
				var longArg = f.Attributes("long").First().Value;
				var shortArg = f.Attributes("short").First().Value;
				var desc = f.Value.Trim();
				if (ArgActionDictionary.ContainsKey(longArg))
				{
					var option = new SearchArgOption(shortArg, longArg, ArgActionDictionary[longArg], desc);
					Options.Add(option);
					ArgDictionary.Add(longArg, option);
					if (!string.IsNullOrWhiteSpace(shortArg))
					{
						ArgDictionary.Add(shortArg, option);
					}
				}
				else if (FlagActionDictionary.ContainsKey(longArg))
				{
					var option = new SearchFlagOption(shortArg, longArg, FlagActionDictionary[longArg], desc);
					Options.Add(option);
					FlagDictionary.Add(longArg, option);
					if (!string.IsNullOrWhiteSpace(shortArg))
					{
						FlagDictionary.Add(shortArg, option);
					}
				}
			}
		}

		public SearchOptions()
		{
			_searchOptionsResource = Properties.Resources.searchoptions;
			Options = new List<SearchOption>();
			ArgDictionary = new Dictionary<string, SearchOption>();
			FlagDictionary = new Dictionary<string, SearchOption>();
			SetOptionsFromXml();
		}

		public SearchSettings SettingsFromArgs(IEnumerable<string> args)
		{
			var settings = new SearchSettings();
			// default to PrintResults = true since this is called from CLI functionality
			settings.PrintResults = true;
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
						throw new SearchException(e.Message);
					}
					if (string.IsNullOrWhiteSpace(s))
					{
						throw new SearchException("Invalid option encountered");
					}
					if (ArgDictionary.ContainsKey(s))
					{
						try
						{
							((SearchArgOption)ArgDictionary[s]).Action(queue.Dequeue(), settings);
						}
						catch (InvalidOperationException e)
						{
							throw new SearchException(e.Message);
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
							throw new SearchException(e.Message);
						}
					}
					else
					{
						throw new SearchException("Unknown option: " + s);
					}
				}
				else
				{
					settings.StartPath = s;
				}
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
			sb.AppendLine(" CsSearch.exe [options] -s <searchpattern> <startpath>\n");
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
