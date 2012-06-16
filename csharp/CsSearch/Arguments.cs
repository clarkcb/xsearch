using System;
using System.Collections.Generic;

namespace CsSearch
{
	class Arguments
	{
		public static SearchSettings SettingsFromArgs(IEnumerable<string> args)
		{
			var settings = new SearchSettings();
			var queue = new Queue<string>(args);
			var actionDictionary = new Dictionary<string, Action<string>>
			                       	{
			                       		{"d", settings.AddInDirPattern},
			                       		{"D", settings.AddOutDirPattern},
			                       		{"f", settings.AddInFilePattern},
			                       		{"F", settings.AddOutFilePattern},
			                       		{"s", settings.AddSearchPattern},
			                       		{"x", settings.AddInExtension},
			                       		{"X", settings.AddOutExtension},
			                       	};
			var flagDictionary = new Dictionary<string, Action>
			                     	{
			                     		{"listfiles", () => settings.ListFiles = true},
			                       		{"t", () => settings.DoTiming = true},
			                       		{"v", () => settings.Verbose = true},
			                     	};

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
					catch (InvalidOperationException)
					{
						throw new SearchArgumentException();
					}
					if (string.IsNullOrWhiteSpace(s))
					{
						throw new SearchArgumentException();
					}
					if (actionDictionary.ContainsKey(s))
					{
						try
						{
							actionDictionary[s](queue.Dequeue());
						}
						catch (InvalidOperationException)
						{
							throw new SearchArgumentException();
						}
					}
					else if (flagDictionary.ContainsKey(s))
					{
						try
						{
							flagDictionary[s]();
						}
						catch (InvalidOperationException)
						{
							throw new SearchArgumentException();
						}
					}
					else
					{
						Console.WriteLine("ERROR: Unknown option: " + s);
						throw new SearchArgumentException();
					}
				}
				else
				{
					settings.StartPath = s;
				}
			}
			if (settings.StartPath == null)
			{
				throw new SearchArgumentException();
			}
			if (settings.SearchPatterns.Count < 1)
			{
				Console.WriteLine("ERROR: You must specify a search string");
				throw new SearchArgumentException();
			}
			return settings;
		}
	}
}
