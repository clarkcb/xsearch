/*******************************************************************************
SearchOptions

Class to encapsulate all command line search options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import java.io.InputStream;
import java.io.IOException;
import java.util.*;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class SearchOptions {
	private String searchOptionsXmlPath;
	private List<ISearchOption> options;
	private Map<String, ISearchOption> argMap;
	private Map<String, ISearchOption> flagMap;

    Map<String,SearchArgSetter> argActionMap = new HashMap<String,SearchArgSetter>() {
        {
            put("in-dir", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addInDirPattern(arg);
                }
            });
            put("out-dir", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addOutDirPattern(arg);
                }
            });
            put("in-ext", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addInExtension(arg);
                }
            });
            put("out-ext", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addOutExtension(arg);
                }
            });
            put("in-file", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addInFilePattern(arg);
                }
            });
            put("out-file", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addOutFilePattern(arg);
                }
            });
            put("search", new SearchArgSetter() {
                @Override public void setArg(String arg, SearchSettings settings) {
                    settings.addSearchPattern(arg);
                }
            });
        }
    };

    Map<String,SearchFlagSetter> flagActionMap = new HashMap<String,SearchFlagSetter>() {
        {
            put("firstmatch", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setFirstMatch(true);
                }
            });
            put("allmatches", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setFirstMatch(false);
                }
            });
            put("debug", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setDebug(true);
                }
            });
            put("help", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setPrintUsage(true);
                }
            });
            put("listfiles", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setListFiles(true);
                }
            });
            put("listlines", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setListLines(true);
                }
            });
            put("printmatches", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setPrintResults(true);
                }
            });
            put("noprintmatches", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setPrintResults(false);
                }
            });
            put("dotiming", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setDoTiming(true);
                }
            });
            put("verbose", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setVerbose(true);
                }
            });
            put("version", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setPrintVersion(true);
                }
            });
            put("searchcompressed", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setSearchCompressed(true);
                }
            });
            put("nosearchcompressed", new SearchFlagSetter() {
                @Override public void setFlag(SearchSettings settings) {
                    settings.setSearchCompressed(true);
                }
            });
        }
    };

    private void setOptionsFromXml() {
        InputStream searchOptionsInputStream = getClass().getResourceAsStream(searchOptionsXmlPath);
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(searchOptionsInputStream);
            doc.getDocumentElement().normalize();
            NodeList searchOptionNodes = doc.getElementsByTagName("searchoption");
            for (int i = 0; i < searchOptionNodes.getLength(); i++) {
                Node searchOptionNode = searchOptionNodes.item(i);
                String longArg = searchOptionNode.getAttributes().getNamedItem("long").getNodeValue();
                String shortArg = searchOptionNode.getAttributes().getNamedItem("short").getNodeValue();
                String desc = searchOptionNode.getTextContent().trim();
                if (argActionMap.containsKey(longArg)) {
                    ISearchOption option = new SearchArgOption(shortArg, longArg, desc, argActionMap.get(longArg));
                    options.add(option);
                    argMap.put(longArg, option);
                    if (null != shortArg && !shortArg.equals("")) {
                        argMap.put(shortArg, option);
                    }
                } else if (flagActionMap.containsKey(longArg)) {
                    ISearchOption option = new SearchFlagOption(shortArg, longArg, desc, flagActionMap.get(longArg));
                    options.add(option);
                    flagMap.put(longArg, option);
                    if (null != shortArg && !shortArg.equals("")) {
                        flagMap.put(shortArg, option);
                    }
                }
            }
        } catch (ParserConfigurationException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (SAXException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    public SearchOptions() {
        searchOptionsXmlPath = "/searchoptions.xml";
		options = new ArrayList<ISearchOption>();
        argMap = new HashMap<String, ISearchOption>();
        flagMap = new HashMap<String, ISearchOption>();
        setOptionsFromXml();
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
				optString.append("-").append(shortArg).append(",");
			}
			optString.append("--").append(opt.getLongArg());
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
