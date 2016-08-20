using System;

namespace CsSearch
{
	public class SearchOption
	{
		public string ShortArg { get; private set; }
		public string LongArg { get; private set; }
		public string SortArg
		{
			get
			{
				var longArg = LongArg.Replace("in-", "ina");
				if (!string.IsNullOrWhiteSpace(ShortArg))
					return ShortArg.ToLower() + "a" + longArg;
				return longArg;
			}
		}
		public string Description { get; private set; }

		public SearchOption(string shortArg, string longArg, string description)
		{
			ShortArg = shortArg;
			LongArg = longArg;
			Description = description;
		}
	}

	class SearchArgOption : SearchOption
	{
		public Action<string, SearchSettings> Action { get; private set; }
		public SearchArgOption(string shortArg, string longArg, Action<string, SearchSettings> action, string description) :
			base(shortArg, longArg, description)
		{
			Action = action;
		}
	}

	class SearchFlagOption : SearchOption
	{
		public Action<bool, SearchSettings> Action { get; private set; }
		public SearchFlagOption(string shortArg, string longArg, Action<bool, SearchSettings> action, string description) :
			base(shortArg, longArg, description)
		{
			Action = action;
		}
	}
}
