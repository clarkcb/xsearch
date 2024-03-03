﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using CsFindLib;
using SearchOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,string>>>;

namespace CsSearchLib;

public class SearchOptions
{
	private readonly string _searchOptionsResource;

	private static readonly Dictionary<string, Action<string, SearchSettings>> ArgActionDictionary =
		new()
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
			{ "maxdepth", (s, settings) => settings.MaxDepth = int.Parse(s) },
			{ "maxlastmod", (s, settings) => {
					settings.MaxLastMod = DateTime.Parse(s);
				}
			},
			{ "maxlinelength", (s, settings) => settings.MaxLineLength = int.Parse(s) },
			{ "maxsize", (s, settings) => settings.MaxSize = int.Parse(s) },
			{ "mindepth", (s, settings) => settings.MinDepth = int.Parse(s) },
			{ "minlastmod", (s, settings) => {
					settings.MinLastMod = DateTime.Parse(s);
				}
			},
			{ "minsize", (s, settings) => settings.MinSize = int.Parse(s) },
			{ "out-archiveext", (s, settings) => settings.AddOutArchiveExtension(s) },
			{ "out-archivefilepattern", (s, settings) => settings.AddOutArchiveFilePattern(s) },
			{ "out-dirpattern", (s, settings) => settings.AddOutDirPattern(s) },
			{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
			{ "out-filepattern", (s, settings) => settings.AddOutFilePattern(s) },
			{ "out-filetype", (s, settings) => settings.AddOutFileType(s) },
			{ "out-linesafterpattern", (s, settings) => settings.AddOutLinesAfterPattern(s) },
			{ "out-linesbeforepattern", (s, settings) => settings.AddOutLinesBeforePattern(s) },
			{ "path", (s, settings) => settings.Paths.Add(s) },
			{ "searchpattern", (s, settings) => settings.AddSearchPattern(s) },
			{ "settings-file", SettingsFromFile },
			{ "sort-by", (s, settings) => settings.SetSortBy(s) },
		};

	private static readonly Dictionary<string, Action<bool, SearchSettings>> BoolFlagActionDictionary =
		new()
		{
			{ "allmatches", (b, settings) => settings.FirstMatch = !b },
			{ "archivesonly", (b, settings) => settings.ArchivesOnly = b },
			{ "colorize", (b, settings) => settings.Colorize = b },
			{ "debug", (b, settings) => settings.Debug = b },
			{ "excludehidden", (b, settings) => settings.IncludeHidden = !b },
			{ "firstmatch", (b, settings) => settings.FirstMatch = b },
			{ "help", (b, settings) => settings.PrintUsage = b },
			{ "includehidden", (b, settings) => settings.IncludeHidden = b },
			{ "multilinesearch", (b, settings) => settings.MultiLineSearch = b },
			{ "nocolorize", (b, settings) => settings.Colorize = !b },
			{ "noprintdirs", (b, settings) => settings.PrintDirs = !b },
			{ "noprintfiles", (b, settings) => settings.PrintFiles = !b },
			{ "noprintlines", (b, settings) => settings.PrintLines = !b },
			{ "noprintmatches", (b, settings) => settings.PrintResults = !b },
			{ "norecursive", (b, settings) => settings.Recursive = !b },
			{ "nosearcharchives", (b, settings) => settings.SearchArchives = !b },
			{ "printdirs", (b, settings) => settings.PrintDirs = b },
			{ "printfiles", (b, settings) => settings.PrintFiles = b },
			{ "printlines", (b, settings) => settings.PrintLines = b },
			{ "printmatches", (b, settings) => settings.PrintResults = b },
			{ "recursive", (b, settings) => settings.Recursive = b },
			{ "searcharchives", (b, settings) => settings.SearchArchives = b },
			{ "sort-ascending", (b, settings) => settings.SortDescending = !b },
			{ "sort-caseinsensitive", (b, settings) => settings.SortCaseInsensitive = b },
			{ "sort-casesensitive", (b, settings) => settings.SortCaseInsensitive = !b },
			{ "sort-descending", (b, settings) => settings.SortDescending = b },
			{ "uniquelines", (b, settings) => settings.UniqueLines = b },
			{ "verbose", (b, settings) => settings.Verbose = b },
			{ "version", (b, settings) => settings.PrintVersion = b },
		};

	public List<SearchOption> Options { get; }
	public Dictionary<string, SearchOption> ArgDictionary { get; }
	public Dictionary<string, SearchOption> FlagDictionary { get; }

	public SearchOptions()
	{
		_searchOptionsResource = EmbeddedResource.GetResourceFileContents("CsSearchLib.Resources.searchoptions.json");
		Options = new List<SearchOption>();
		ArgDictionary = new Dictionary<string, SearchOption>();
		FlagDictionary = new Dictionary<string, SearchOption>();
		SetOptionsFromJson();
	}

	private void SetOptionsFromJson()
	{
		var searchOptionsDict = JsonSerializer.Deserialize<SearchOptionsDictionary>(_searchOptionsResource);
		if (searchOptionsDict == null || !searchOptionsDict.ContainsKey("searchoptions"))
		{
			throw new SearchException("Missing or invalid search options resource");
		}
		var optionDicts = searchOptionsDict["searchoptions"];
		foreach (var optionDict in optionDicts)
		{
			var longArg = optionDict["long"];
			var shortArg = optionDict.TryGetValue("short", out var value) ? value : null;
			var desc = optionDict["desc"];
			if (ArgActionDictionary.TryGetValue(longArg, out var argVal))
			{
				var option = new SearchArgOption(shortArg, longArg, argVal, desc);
				Options.Add(option);
				ArgDictionary.Add(longArg, option);
				if (!string.IsNullOrWhiteSpace(shortArg))
				{
					ArgDictionary.Add(shortArg, option);
				}
			}
			else if (BoolFlagActionDictionary.TryGetValue(longArg, out var flagVal))
			{
				var option = new SearchFlagOption(shortArg, longArg, flagVal, desc);
				Options.Add(option);
				FlagDictionary.Add(longArg, option);
				if (!string.IsNullOrWhiteSpace(shortArg))
				{
					FlagDictionary.Add(shortArg, option);
				}
			}
		}
	}

	private static void SettingsFromFile(string filePath, SearchSettings settings)
	{
		var fileInfo = new FileInfo(filePath);
		if (!fileInfo.Exists)
			throw new SearchException("Settings fie not found: " + filePath);
		var contents = FileUtil.GetFileContents(filePath, Encoding.Default);
		SettingsFromJson(contents, settings);
	}

	public static void SettingsFromJson(string jsonString, SearchSettings settings)
	{
		var settingsDict = JsonSerializer.Deserialize<Dictionary<string, object>>(jsonString);
		if (settingsDict == null) return;
		foreach (var (key, value) in settingsDict)
		{
			var obj = (JsonElement)value;
			ApplySetting(key, obj, settings);
		}
	}

	private static void ApplySetting(string arg, JsonElement obj, SearchSettings settings)
	{
		switch (obj.ValueKind)
		{
			case JsonValueKind.String:
				var str = obj.GetString();
				if (str != null)
				{
					ApplySetting(arg, str, settings);
				}
				break;
			case JsonValueKind.True:
				ApplySetting(arg, true, settings);
				break;
			case JsonValueKind.False:
				ApplySetting(arg, false, settings);
				break;
			case JsonValueKind.Number:
				ApplySetting(arg, obj.GetInt32().ToString(), settings);
				break;
			case JsonValueKind.Array:
				foreach (var arrVal in obj.EnumerateArray())
				{
					ApplySetting(arg, arrVal, settings);
				}
				break;
			case JsonValueKind.Undefined:
			case JsonValueKind.Object:
			case JsonValueKind.Null:
			default:
				break;
		}
	}

	private static void ApplySetting(string arg, string val, SearchSettings settings)
	{
		if (ArgActionDictionary.TryGetValue(arg, out var value))
		{
			value(val, settings);
		}
		else if (arg.Equals("path"))
		{
			settings.Paths.Add(val);
		}
		else
		{
			throw new SearchException("Invalid option: " + arg);
		}
	}

	private static void ApplySetting(string arg, bool val, SearchSettings settings)
	{
		if (BoolFlagActionDictionary.TryGetValue(arg, out var value))
		{
			value(val, settings);
		}
		else
		{
			throw new SearchException("Invalid option: " + arg);
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
				if (ArgDictionary.TryGetValue(s, out var argVal))
				{
					try
					{
						((SearchArgOption)argVal).Action(queue.Dequeue(), settings);
					}
					catch (InvalidOperationException e)
					{
						throw new SearchException(e.Message);
					}
				}
				else if (FlagDictionary.TryGetValue(s, out var flagVal))
				{
					try
					{
						((SearchFlagOption)flagVal).Action(true, settings);
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
				settings.Paths.Add(s);
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
		sb.AppendLine(" cssearch [options] -s <searchpattern> <path> [<path> ...]\n");
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
