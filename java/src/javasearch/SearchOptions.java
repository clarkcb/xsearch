/*******************************************************************************
SearchOptions

Class to encapsulate all command line search options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public class SearchOptions {

	private List<ISearchOption> options;
	private Map<String, ISearchOption> argMap;
	private Map<String, ISearchOption> flagMap;

	private static List<ISearchOption> argOptions = 
		new ArrayList<ISearchOption>(
			Arrays.asList(
				new SearchArgOption("d", "dirname",
					"Specify name pattern for directories to include in search",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addInDirPattern(arg);
						}
					}),
				new SearchArgOption("D", "dirfilter",
					"Specify name pattern for directories to exclude from search",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addOutDirPattern(arg);
						}
					}),
				new SearchArgOption("f", "filename",
					"Specify name pattern for files to include in search",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addInFilePattern(arg);
						}
					}),
				new SearchArgOption("F", "filefilter",
					"Specify name pattern for files to exclude from search",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addOutFilePattern(arg);
						}
					}),
				new SearchArgOption("s", "search",
					"Specify search pattern",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addSearchPattern(arg);
						}
					}),
				new SearchArgOption("x", "ext",
					"Specify extension of files to include in search",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addInExtension(arg);
						}
					}),
				new SearchArgOption("X", "extfilter",
					"Specify extension of files to exclude from search",
					new SearchArgSetter() {
						@Override public void setArg(String arg, SearchSettings settings) {
							settings.addOutExtension(arg);
						}
					})
			)
		);

	private static List<ISearchOption> flagOptions = 
		new ArrayList<ISearchOption>(
			Arrays.asList(
				new SearchFlagOption("1", "firstmatch",
					"Capture only the first match for a file+search combination",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setFirstMatch(true);
						}
					}),
				new SearchFlagOption("a", "allmatches",
					"Capture all matches*",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setFirstMatch(false);
						}
					}),
				new SearchFlagOption("", "debug",
					"Set output mode to debug",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setDebug(true);
						}
					}),
				new SearchFlagOption("h", "help",
					"Print usage and exit",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setPrintUsage(true);
						}
					}),
				new SearchFlagOption("", "listfiles",
					"Generate a list of the matching files after searching",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setListFiles(true);
						}
					}),
				new SearchFlagOption("", "listlines",
					"Generate a list of the matching lines after searching",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setListLines(true);
						}
					}),
				new SearchFlagOption("p", "printmatches",
					"Print matches to stdout as found*",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setPrintResults(true);
						}
					}),
				new SearchFlagOption("P", "noprintmatches",
					"Suppress print of matches to stdout",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setPrintResults(false);
						}
					}),
				new SearchFlagOption("t", "dotiming",
					"Time search execution",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setDoTiming(true);
						}
					}),
				new SearchFlagOption("v", "verbose",
					"Specify verbose output",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setVerbose(true);
						}
					}),
				new SearchFlagOption("V", "version",
					"Print version and exit",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setPrintVersion(true);
						}
					}),
				new SearchFlagOption("z", "searchcompressed",
					"Search compressed files (bz2, gz, tar, zip)*",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setSearchCompressed(true);
						}
					}),
				new SearchFlagOption("Z", "nosearchcompressed",
					"Do not search compressed files (bz2, gz, tar, zip)",
					new SearchFlagSetter() {
						@Override public void setFlag(SearchSettings settings) {
							settings.setSearchCompressed(true);
						}
					})
			)
		);

	private Map<String, ISearchOption> mapFromOptions(List<ISearchOption> options) {
		Map<String, ISearchOption> map = new HashMap<String, ISearchOption>();
		try {
			for (ISearchOption opt : options) {
				String shortArg = opt.getShortArg();
				if (null != shortArg && !shortArg.equals("")) {
					map.put(shortArg, opt);
				}
				map.put(opt.getLongArg(), opt);
			}
		} catch (Exception e) {
			System.out.println("Exception: " + e.toString());
		}
		return map;
	}

	public SearchOptions() {
		this.options = new ArrayList<ISearchOption>(argOptions);
		this.options.addAll(flagOptions);
		this.argMap = this.mapFromOptions(argOptions);
		this.flagMap = this.mapFromOptions(flagOptions);
	}

	public SearchSettings settingsFromArgs(String[] args) throws Exception {
		SearchSettings settings = new SearchSettings();
		Queue<String> queue = new LinkedList<String>(Arrays.asList(args));
		while (!queue.isEmpty()) {
			String arg = queue.remove();
			if (arg.startsWith("-")) {
				while (arg.length() > 0 && arg.startsWith("-")) {
					arg = arg.substring(1);
				}
				if (this.argMap.containsKey(arg)) {
					if (!queue.isEmpty()) {
						String argVal = queue.remove();
						SearchArgOption option = (SearchArgOption)this.argMap.get(arg);
						option.setArg(argVal, settings);
					} else {
						throw new Exception("Missing value for option " + arg);
					}
				} else if (this.flagMap.containsKey(arg)) {
					SearchFlagOption option = (SearchFlagOption)this.flagMap.get(arg);
					option.setFlag(settings);
				} else {
					throw new Exception("Undefined option: " + arg);
				}
			} else {
				settings.setStartPath(arg);
			}
		}
		if (null == settings.getStartPath() || settings.getStartPath().equals("")) {
			throw new Exception("Startpath not defined");
		}
		if (settings.getSearchPatterns().isEmpty()) {
			throw new Exception("No search patterns defined");
		}
		return settings;
	}

	public void usage(int exitStatus) {
		System.out.println(this.getUsageString());
		System.exit(exitStatus);
	}

	private static Comparator<ISearchOption> searchOptionComparator = new Comparator<ISearchOption>() {
		public int compare(ISearchOption s1, ISearchOption s2)
		{
		    return s1.getSortArg().toLowerCase().compareTo(s2.getSortArg().toLowerCase());
		}
	};

	public String getUsageString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Usage:\n");
		sb.append(" javasearch [options] <startpath>\n\n");
		sb.append("Options:\n");

		Collections.sort(this.options, searchOptionComparator);

		List<String> optStrings = new ArrayList<String>();
		List<String> optDescs = new ArrayList<String>();
		int longest = 0;
		for (ISearchOption opt : this.options) {
			StringBuilder optString = new StringBuilder();
			String shortArg = opt.getShortArg();
			if (null != shortArg && !shortArg.equals("")) {
				optString.append("-" + shortArg + ",");
			}
			optString.append("--" + opt.getLongArg());
			if (optString.length() > longest) {
				longest = optString.length();
			}
			optStrings.add(optString.toString());
			optDescs.add(opt.getDescription());
		}
		//String format = " {0,-"+longest+"}  {1}";
		//%[argument_index$][flags][width][.precision]conversion
		String format = " %1$-"+longest+"s  %2$s\n";
		for (int i = 0; i < optStrings.size(); i++) {
			sb.append(String.format(format, optStrings.get(i), optDescs.get(i)));
		}
		return sb.toString();
	}
}
