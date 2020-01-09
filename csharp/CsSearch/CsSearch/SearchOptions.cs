using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Xml.Linq;

namespace CsSearch
{
	public class SearchOptions
	{
		private readonly string _searchOptionsResource;

		public static Dictionary<string, Action<string, SearchSettings>> ArgActionDictionary =
			new Dictionary<string, Action<string,SearchSettings>>
				{
					{ "encoding", (s, settings) => settings.TextFileEncoding = s },
					{ "in-archiveext", (s, settings) => settings.AddInArchiveExtension(s) },
					{ "in-archivefilepattern", (s, settings) => settings.AddInArchiveFilePattern(s) },
					{ "in-dirpattern", (s, settings) => settings.AddInDirPattern(s) },
					{ "in-ext", (s, settings) => settings.AddInExtension(s) },
					{ "in-filepattern", (s, settings) => settings.AddInFilePattern(s) },
					{ "in-filetype", (s, settings) => settings.AddInFileType(s) },
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
					{ "out-filetype", (s, settings) => settings.AddOutFileType(s) },
					{ "out-linesafterpattern", (s, settings) => settings.AddOutLinesAfterPattern(s) },
					{ "out-linesbeforepattern", (s, settings) => settings.AddOutLinesBeforePattern(s) },
					{ "search", (s, settings) => settings.AddSearchPattern(s) },
				};

		public static Dictionary<string, Action<bool, SearchSettings>> BoolFlagActionDictionary =
			new Dictionary<string, Action<bool, SearchSettings>>
				{
					{ "allmatches", (b, settings) => settings.FirstMatch = !b },
					{ "archivesonly", (b, settings) => settings.SetArchivesOnly(b) },
					{ "debug", (b, settings) => settings.SetDebug(b) },
					{ "excludehidden", (b, settings) => settings.ExcludeHidden = b },
					{ "firstmatch", (b, settings) => settings.FirstMatch = b },
					{ "help", (b, settings) => settings.PrintUsage = b },
					{ "includehidden", (b, settings) => settings.ExcludeHidden = !b },
					{ "listdirs", (b, settings) => settings.ListDirs = b },
					{ "listfiles", (b, settings) => settings.ListFiles = b },
					{ "listlines", (b, settings) => settings.ListLines = b },
					{ "multilinesearch", (b, settings) => settings.MultiLineSearch = b },
					{ "noprintmatches", (b, settings) => settings.PrintResults = !b },
					{ "norecursive", (b, settings) => settings.Recursive = !b },
					{ "nosearcharchives", (b, settings) => settings.SearchArchives = !b },
					{ "printmatches", (b, settings) => settings.PrintResults = b },
					{ "recursive", (b, settings) => settings.Recursive = b },
					{ "searcharchives", (b, settings) => settings.SearchArchives = b },
					{ "uniquelines", (b, settings) => settings.UniqueLines = b },
					{ "verbose", (b, settings) => settings.Verbose = b },
					{ "version", (b, settings) => settings.PrintVersion = b },
				};

		public List<SearchOption> Options { get; private set; }
		public Dictionary<string, SearchOption> ArgDictionary { get; private set; }
		public Dictionary<string, SearchOption> FlagDictionary { get; private set; }

		public SearchOptions()
		{
			_searchOptionsResource = EmbeddedResource.GetResourceFileContents("CsSearch.Resources.searchoptions.xml");
			Options = new List<SearchOption>();
			ArgDictionary = new Dictionary<string, SearchOption>();
			FlagDictionary = new Dictionary<string, SearchOption>();
			SetOptionsFromXml();
		}

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
				else if (BoolFlagActionDictionary.ContainsKey(longArg))
				{
					var option = new SearchFlagOption(shortArg, longArg, BoolFlagActionDictionary[longArg], desc);
					Options.Add(option);
					FlagDictionary.Add(longArg, option);
					if (!string.IsNullOrWhiteSpace(shortArg))
					{
						FlagDictionary.Add(shortArg, option);
					}
				}
			}
		}

		public SearchSettings SettingsFromArgs(IEnumerable<string> args)
		{
			// default to PrintResults = true since this is called from CLI functionality
			var settings = new SearchSettings {PrintResults = true};
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
						throw new SearchException("Invalid option: -");
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
							((SearchFlagOption)FlagDictionary[s]).Action(true, settings);
						}
						catch (InvalidOperationException e)
						{
							throw new SearchException(e.Message);
						}
					}
					else
					{
						throw new SearchException("Invalid option: " + s);
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
