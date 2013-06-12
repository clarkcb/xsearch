using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace CsSearch
{
	class SearchOptions
	{
		private readonly FileInfo _searchOptionsPath;

		public static Dictionary<string, Action<string, SearchSettings>> ArgActionDictionary =
			new Dictionary<string,Action<string,SearchSettings>>
				{
					{ "in-dir", (s, settings) => settings.AddInDirPattern(s) },
					{ "in-ext", (s, settings) => settings.AddInExtension(s) },
					{ "in-file", (s, settings) => settings.AddInFilePattern(s) },
					//{ "in-linesafterpattern", (s, settings) => settings.linesaftersearches.append(s) },
					//{ "in-linesbeforepattern", (s, settings) => settings.linesbeforesearches.append(s) },
					//{ "linesafter", (s, settings) => settings.SetProperty("numlinesafter", int(s)) },
					//{ "linesbefore", (s, settings) => settings.SetProperty("numlinesbefore", int(s)) },
					{ "out-dir", (s, settings) => settings.AddOutDirPattern(s) },
					{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
					{ "out-file", (s, settings) => settings.AddOutFilePattern(s) },
					//{ "out-linesafterpattern", (s, settings) => settings.linesafterfilters.append(s) },
					//{ "out-linesbeforepattern", (s, settings) => settings.linesbeforefilters.append(s) },
					{ "search", (s, settings) => settings.AddSearchPattern(s) },
				};

		public static Dictionary<string, Action<SearchSettings>> FlagActionDictionary =
			new Dictionary<string,Action<SearchSettings>>
				{
					{ "allmatches", settings => settings.FirstMatch = false },
					{ "debug", settings => settings.Debug = true },
					{ "dotiming", settings => settings.DoTiming = true },
					{ "firstmatch", settings => settings.FirstMatch = true },
					{ "help", settings => settings.PrintUsage = true },
					{ "listfiles", settings => settings.ListFiles = true },
					{ "listlines", settings => settings.ListLines = true },
					{ "noprintmatches", settings => settings.PrintResults = false },
					{ "nosearchcompressed", settings => settings.SearchCompressed = false },
					{ "printmatches", settings => settings.PrintResults = true },
					{ "searchcompressed", settings => settings.SearchCompressed = true },
					{ "verbose", settings => settings.Verbose = true },
					{ "version", settings => settings.PrintVersion = true },
				};

		public List<SearchOption> Options { get; private set; }
		public Dictionary<string, SearchOption> ArgDictionary { get; private set; }
		public Dictionary<string, SearchOption> FlagDictionary { get; private set; }

		private void SetOptionsFromXml()
		{
			var doc = XDocument.Load(new StreamReader(_searchOptionsPath.FullName));
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
			var appSettings = new Properties.Settings();
			_searchOptionsPath = new FileInfo(appSettings.SearchOptionsPath);
			Options = new List<SearchOption>();
			ArgDictionary = new Dictionary<string, SearchOption>();
			FlagDictionary = new Dictionary<string, SearchOption>();
			SetOptionsFromXml();
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
