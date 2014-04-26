package xsearch

func GetSearchOptions() *SearchOptions {
	return &SearchOptions{
		[]SearchOption{
			SearchOption{"a", "allmatches", "Capture all matches*"},
			SearchOption{"", "debug", "Set output mode to debug"},
			SearchOption{"t", "dotiming", "Measure search execution duration"},
			SearchOption{"1", "firstmatch", "Capture only the first match for a file+search combination"},
			SearchOption{"h", "help", "Print this usage and exit"},
			SearchOption{"d", "in-dirpattern", "Specify name pattern for directories to include in search"},
			SearchOption{"x", "in-ext", "Specify extension for files to include in search"},
			SearchOption{"f", "in-filepattern", "Specify name pattern for files to include in search"},
			SearchOption{"", "in-linesafterpattern", "Specify pattern to search the \"lines-after\" lines on (used with --linesafter)"},
			SearchOption{"", "in-linesbeforepattern", "Specify pattern to search the \"lines-before\" lines on (used with --linesbefore)"},
			SearchOption{"B", "linesafter", "Number of lines to include after every matched line (default: 0)"},
			SearchOption{"", "linesaftertopattern", "Specify pattern to collect lines after up to where pattern is matched (or EOF)"},
			SearchOption{"", "linesafteruntilpattern", "Specify pattern to collect lines after until pattern is matched (or EOF)"},
			SearchOption{"b", "linesbefore", "Number of lines to include before every matched line (default: 0)"},
			SearchOption{"", "listdirs", "Generate a list of the matching directories after searching"},
			SearchOption{"", "listfiles", "Generate a list of the matching files after searching"},
			SearchOption{"", "listlines", "Generate a list of the matching lines after searching"},
			SearchOption{"m", "multilinesearch", "Search files as single multi-line content block"},
			SearchOption{"P", "noprintmatches", "Suppress printing of matches to stdout"},
			SearchOption{"Z", "nosearchcompressed", "Do not search compressed files (bz2, gz, tar, zip)"},
			SearchOption{"D", "out-dirpattern", "Specify name pattern for directories to exclude from search"},
			SearchOption{"X", "out-ext", "Specify extension for files to exclude from search"},
			SearchOption{"F", "out-filepattern", "Specify name pattern for files to exclude from search"},
			SearchOption{"", "out-linesafterpattern", "Specify pattern to filter the \"lines-after\" lines on (used with --linesafter)"},
			SearchOption{"", "out-linesbeforepattern", "Specify pattern to filter the \"lines-before\" lines on (used with --linesbefore)"},
			SearchOption{"p", "printmatches", "Print matches to stdout"},
			SearchOption{"s", "search", "Specify search pattern"},
			SearchOption{"z", "searchcompressed", "Search compressed files (bz2, gz, tar, zip)*"},
			SearchOption{"v", "verbose", "Set output mode to verbose"},
			SearchOption{"V", "version", "Print version and exit"},
		},
	}
}
