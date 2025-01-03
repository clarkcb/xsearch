using System;
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

	private static readonly Dictionary<string, Action<bool, SearchSettings>> BoolActionDictionary =
		new()
		{
			{ "allmatches", (b, settings) => settings.FirstMatch = !b },
			{ "archivesonly", (b, settings) => settings.ArchivesOnly = b },
			{ "colorize", (b, settings) => settings.Colorize = b },
			{ "debug", (b, settings) => settings.Debug = b },
			{ "excludehidden", (b, settings) => settings.IncludeHidden = !b },
			{ "firstmatch", (b, settings) => settings.FirstMatch = b },
			{ "followsymlinks", (b, settings) => settings.FollowSymlinks = b },
			{ "help", (b, settings) => settings.PrintUsage = b },
			{ "includehidden", (b, settings) => settings.IncludeHidden = b },
			{ "multilinesearch", (b, settings) => settings.MultiLineSearch = b },
			{ "nocolorize", (b, settings) => settings.Colorize = !b },
			{ "nofollowsymlinks", (b, settings) => settings.FollowSymlinks = !b },
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

	private static readonly Dictionary<string, Action<string, SearchSettings>> StringActionDictionary =
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
			{ "linesaftertopattern", (s, settings) => settings.AddLinesAfterToPattern(s) },
			{ "linesafteruntilpattern", (s, settings) => settings.AddLinesAfterUntilPattern(s) },
			{ "maxlastmod", (s, settings) => {
					settings.MaxLastMod = DateTime.Parse(s);
				}
			},
			{ "minlastmod", (s, settings) => {
					settings.MinLastMod = DateTime.Parse(s);
				}
			},
			{ "out-archiveext", (s, settings) => settings.AddOutArchiveExtension(s) },
			{ "out-archivefilepattern", (s, settings) => settings.AddOutArchiveFilePattern(s) },
			{ "out-dirpattern", (s, settings) => settings.AddOutDirPattern(s) },
			{ "out-ext", (s, settings) => settings.AddOutExtension(s) },
			{ "out-filepattern", (s, settings) => settings.AddOutFilePattern(s) },
			{ "out-filetype", (s, settings) => settings.AddOutFileType(s) },
			{ "out-linesafterpattern", (s, settings) => settings.AddOutLinesAfterPattern(s) },
			{ "out-linesbeforepattern", (s, settings) => settings.AddOutLinesBeforePattern(s) },
			{ "path", (s, settings) => settings.AddPath(s) },
			{ "searchpattern", (s, settings) => settings.AddSearchPattern(s) },
			{ "sort-by", (s, settings) => settings.SetSortBy(s) },
		};

	private static readonly Dictionary<string, Action<int, SearchSettings>> IntActionDictionary =
		new()
		{
			{ "linesafter", (i, settings) => settings.LinesAfter = i },
			{ "linesbefore", (i, settings) => settings.LinesBefore = i },
			{ "maxdepth", (i, settings) => settings.MaxDepth = i },
			{ "maxlinelength", (i, settings) => settings.MaxLineLength = i },
			{ "maxsize", (i, settings) => settings.MaxSize = i },
			{ "mindepth", (i, settings) => settings.MinDepth = i },
			{ "minsize", (i, settings) => settings.MinSize = i },
		};

	public List<SearchOption> Options { get; }
	// public Dictionary<string, SearchOption> BoolOptionDictionary { get; }
	// public Dictionary<string, SearchOption> StringOptionDictionary { get; }
	// public Dictionary<string, SearchOption> IntOptionDictionary { get; }
	private Dictionary<string, string> LongArgDictionary { get; }

	public SearchOptions()
	{
		_searchOptionsResource = EmbeddedResource.GetResourceFileContents("CsSearchLib.Resources.searchoptions.json");
		Options = new List<SearchOption>();
		LongArgDictionary = new Dictionary<string, string>
		{
			{ "path", "path" }
		};
		SetOptionsFromJson();
	}

	private void SetOptionsFromJson()
	{
		var searchOptionsDict = JsonSerializer.Deserialize<SearchOptionsDictionary>(_searchOptionsResource);
		if (searchOptionsDict == null
		    || !searchOptionsDict.TryGetValue("searchoptions", out List<Dictionary<string, string>>? optionDicts))
		{
			throw new SearchException("Missing or invalid search options resource");
		}

        foreach (var optionDict in optionDicts)
		{
			var longArg = optionDict["long"];
			LongArgDictionary.Add(longArg, longArg);
			string? shortArg = null;
			if (optionDict.TryGetValue("short", out var shortVal))
			{
				shortArg = shortVal;
				LongArgDictionary.Add(shortArg, longArg);
			}
			var desc = optionDict["desc"];
			var option = new SearchOption(shortArg, longArg, desc);
			Options.Add(option);
		}
	}

	private static void ApplySetting(string arg, JsonElement elem, SearchSettings settings)
	{
		if (BoolActionDictionary.TryGetValue(arg, out var boolAction))
		{
			if (elem.ValueKind is JsonValueKind.False or JsonValueKind.True)
			{
				boolAction(elem.GetBoolean(), settings);
			}
			else
			{
				throw new SearchException($"Invalid value for option: {arg}");
			}
		}
		else if (StringActionDictionary.TryGetValue(arg, out var stringAction))
		{
			if (elem.ValueKind is JsonValueKind.String)
			{
				var s = elem.GetString();
				if (s is not null)
				{
					stringAction(s, settings);
				}
				else
				{
					throw new SearchException($"Invalid value for option: {arg}");
				}
			} else if (elem.ValueKind is JsonValueKind.Array)
			{
				foreach (var arrVal in elem.EnumerateArray())
				{
					ApplySetting(arg, arrVal, settings);
				}
			}
			else
			{
				throw new SearchException($"Invalid value for option: {arg}");
			}
		}
		else if (IntActionDictionary.TryGetValue(arg, out var intAction))
		{
			if (elem.ValueKind is JsonValueKind.Number)
			{
				intAction(elem.GetInt32(), settings);
			}
			else
			{
				throw new SearchException($"Invalid value for option: {arg}");
			}
		}
		else
		{
			throw new SearchException($"Invalid option: {arg}");
		}
	}

	public void UpdateSettingsFromJson(string jsonString, SearchSettings settings)
	{
		var settingsDict = JsonSerializer.Deserialize<Dictionary<string, JsonElement>>(jsonString);
		if (settingsDict == null) throw new SearchException($"Unable to parse json");
		var keys = settingsDict.Keys.ToList();
		// keys are sorted so that output is consistent across all versions
		keys.Sort();
		var invalidKeys = keys.Except(LongArgDictionary.Keys).ToList();
		if (invalidKeys.Count > 0)
		{
			throw new SearchException($"Invalid option: {invalidKeys[0]}");
		}
		foreach (var key in keys)
		{
			ApplySetting(key, settingsDict[key], settings);
		}
	}

	private void UpdateSettingsFromFile(string filePath, SearchSettings settings)
	{
		var expandedPath = FileUtil.ExpandPath(filePath);
		var fileInfo = new FileInfo(expandedPath);
		if (!fileInfo.Exists)
			throw new SearchException("Settings file not found: " + filePath);
		if (!fileInfo.Extension.Equals(".json"))
			throw new SearchException($"Invalid settings file (must be JSON): {filePath}");
		var contents = FileUtil.GetFileContents(expandedPath, Encoding.Default);
		try
		{
			UpdateSettingsFromJson(contents, settings);
		}
		catch (JsonException)
		{
			throw new SearchException($"Unable to parse JSON in settings file: {filePath}");
		}
	}

	public SearchSettings SettingsFromArgs(IEnumerable<string> args)
	{
		// default to PrintResults = true since this is called from CLI functionality
		var settings = new SearchSettings {PrintResults = true};
		var queue = new Queue<string>(args);

		while (queue.Count > 0)
		{
			var arg = queue.Dequeue();
			if (arg.StartsWith('-'))
			{
				try
				{
					while (arg.StartsWith('-'))
					{
						arg = arg[1..];
					}
				}
				catch (InvalidOperationException e)
				{
					throw new SearchException(e.Message);
				}
				if (string.IsNullOrWhiteSpace(arg))
				{
					throw new SearchException("Invalid option: -");
				}

				if (LongArgDictionary.TryGetValue(arg, out var longArg))
				{
					if (BoolActionDictionary.TryGetValue(longArg, out var boolAction))
					{
						boolAction(true, settings);
					}
					else if (StringActionDictionary.TryGetValue(longArg, out var stringAction))
					{
						try
						{
							stringAction(queue.Dequeue(), settings);
						}
						catch (InvalidOperationException)
						{
							throw new SearchException($"Missing value for option {arg}");
						}
					}
					else if (IntActionDictionary.TryGetValue(longArg, out var intAction))
					{
						try
						{
							intAction(int.Parse(queue.Dequeue()), settings);
						}
						catch (InvalidOperationException)
						{
							throw new SearchException($"Missing value for option {arg}");
						}
					}
					else if (longArg.Equals("settings-file"))
					{
						try
						{
							UpdateSettingsFromFile(queue.Dequeue(), settings);
						}
						catch (InvalidOperationException)
						{
							throw new SearchException($"Missing value for option {arg}");
						}
					}
					else
					{
						throw new SearchException($"Invalid option: {arg}");
					}
				}
				else
				{
					throw new SearchException($"Invalid option: {arg}");
				}
			}
			else
			{
				settings.AddPath(arg);
			}
		}
		return settings;
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

	public void Usage(int exitCode = 0)
	{
		Console.WriteLine(GetUsageString());
		Environment.Exit(exitCode);
	}
}
