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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class SearchOptions {
    private List<SearchOption> options;

    public SearchOptions() throws ParserConfigurationException, SAXException, IOException {
        options = new ArrayList<>();
        setOptionsFromXml();
    }

    @FunctionalInterface
    private interface ArgSetter {
        void set(String s, SearchSettings settings);
    }

    private Map<String, ArgSetter> argActionMap = new HashMap<String, ArgSetter>(24) {
        {
            put("in-archiveext", (s, settings) -> settings.addInArchiveExtension(s));
            put("in-archivefilepattern", (s, settings) -> settings.addInArchiveFilePattern(s));
            put("in-dirpattern", (s, settings) -> settings.addInDirPattern(s));
            put("in-ext", (s, settings) -> settings.addInExtension(s));
            put("in-filepattern", (s, settings) -> settings.addInFilePattern(s));
            put("in-linesafterpattern", (s, settings) -> settings.addInLinesAfterPattern(s));
            put("in-linesbeforepattern", (s, settings) -> settings.addInLinesBeforePattern(s));
            put("linesafter", (s, settings) -> settings.setLinesAfter(Integer.parseInt(s)));
            put("linesaftertopattern", (s, settings) -> settings.addLinesAfterToPattern(s));
            put("linesafteruntilpattern", (s, settings) -> settings.addLinesAfterUntilPattern(s));
            put("linesbefore", (s, settings) -> settings.setLinesBefore(Integer.parseInt(s)));
            put("maxlinelength", (s, settings) -> settings.setMaxLineLength(Integer.parseInt(s)));
            put("out-archiveext", (s, settings) -> settings.addOutArchiveExtension(s));
            put("out-archivefilepattern", (s, settings) -> settings.addOutArchiveFilePattern(s));
            put("out-dirpattern", (s, settings) -> settings.addOutDirPattern(s));
            put("out-ext", (s, settings) -> settings.addOutExtension(s));
            put("out-filepattern", (s, settings) -> settings.addOutFilePattern(s));
            put("out-linesafterpattern", (s, settings) -> settings.addOutLinesAfterPattern(s));
            put("out-linesbeforepattern", (s, settings) -> settings.addOutLinesBeforePattern(s));
            put("search", (s, settings) -> settings.addSearchPattern(s));
        }
    };

    @FunctionalInterface
    private interface FlagSetter {
        void set(SearchSettings settings);
    }

    private Map<String, FlagSetter> flagActionMap = new HashMap<String, FlagSetter>(24) {
        {
            put("archivesonly", (settings) -> settings.setArchivesOnly(true));
            put("allmatches", (settings) -> settings.setFirstMatch(false));
            put("debug", (settings) -> settings.setDebug(true));
            put("excludehidden", (settings) -> settings.setExcludeHidden(true));
            put("firstmatch", (settings) -> settings.setFirstMatch(true));
            put("help", (settings) -> settings.setPrintUsage(true));
            put("includehidden", (settings) -> settings.setExcludeHidden(false));
            put("listdirs", (settings) -> settings.setListDirs(true));
            put("listfiles", (settings) -> settings.setListFiles(true));
            put("listlines", (settings) -> settings.setListLines(true));
            put("multilinesearch", (settings) -> settings.setMultiLineSearch(true));
            put("noprintmatches", (settings) -> settings.setPrintResults(false));
            put("norecursive", (settings) -> settings.setRecursive(false));
            put("nosearcharchives", (settings) -> settings.setSearchArchives(false));
            put("printmatches", (settings) -> settings.setPrintResults(true));
            put("recursive", (settings) -> settings.setRecursive(true));
            put("searcharchives", (settings) -> settings.setSearchArchives(true));
            put("uniquelines", (settings) -> settings.setUniqueLines(true));
            put("verbose", (settings) -> settings.setVerbose(true));
            put("version", (settings) -> settings.setPrintVersion(true));
        }
    };

    private void setOptionsFromXml() throws ParserConfigurationException, SAXException, IOException {
        final String searchOptionsXmlPath = "/searchoptions.xml";
        InputStream searchOptionsInputStream = getClass().getResourceAsStream(searchOptionsXmlPath);
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.parse(searchOptionsInputStream);
        doc.getDocumentElement().normalize();
        NodeList searchOptionNodes = doc.getElementsByTagName("searchoption");
        for (int i = 0; i < searchOptionNodes.getLength(); i++) {
            Node searchOptionNode = searchOptionNodes.item(i);
            String longArg = searchOptionNode.getAttributes().getNamedItem("long").getNodeValue();
            String shortArg = searchOptionNode.getAttributes().getNamedItem("short").getNodeValue();
            String desc = searchOptionNode.getTextContent().trim();
            options.add(new SearchOption(shortArg, longArg, desc));
        }
    }

    public final SearchSettings settingsFromArgs(final String[] args) throws SearchException {
        SearchSettings settings = new SearchSettings();
        // default printresults to True since running from command line
        settings.setPrintResults(true);

        // add short arg mappings
        options.stream().filter(o -> !o.getShortArg().isEmpty()).forEach(o -> {
            if (argActionMap.containsKey(o.getLongArg())) {
                argActionMap.put(o.getShortArg(), argActionMap.get(o.getLongArg()));
            } else if (flagActionMap.containsKey(o.getLongArg())) {
                flagActionMap.put(o.getShortArg(), flagActionMap.get(o.getLongArg()));
            }
        });

        Queue<String> queue = new LinkedList<>(Arrays.asList(args));
        while (!queue.isEmpty()) {
            String arg = queue.remove();
            if (arg.startsWith("-")) {
                while (arg.length() > 0 && arg.startsWith("-")) {
                    arg = arg.substring(1);
                }
                if (this.argActionMap.containsKey(arg)) {
                    if (!queue.isEmpty()) {
                        String argVal = queue.remove();
                        this.argActionMap.get(arg).set(argVal, settings);
                    } else {
                        throw new SearchException("Missing value for option " + arg);
                    }
                } else if (this.flagActionMap.containsKey(arg)) {
                    this.flagActionMap.get(arg).set(settings);
                } else {
                    throw new SearchException("Invalid option: " + arg);
                }
            } else {
                settings.setStartPath(arg);
            }
        }
        return settings;
    }

    public final void usage(final int exitStatus) {
        System.out.println(this.getUsageString());
        System.exit(exitStatus);
    }

    public final String getUsageString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Usage:\n");
        sb.append(" javasearch [options] -s <searchpattern> <startpath>\n\n");
        sb.append("Options:\n");

        Collections.sort(this.options, (o1, o2) -> o1.getSortArg().compareTo(o2.getSortArg()));

        List<String> optStrings = new ArrayList<>();
        List<String> optDescs = new ArrayList<>();
        int longest = 0;
        for (SearchOption opt : this.options) {
            StringBuilder optString = new StringBuilder();
            String shortArg = opt.getShortArg();
            if (null != shortArg && !shortArg.isEmpty()) {
                optString.append("-").append(shortArg).append(",");
            }
            optString.append("--").append(opt.getLongArg());
            if (optString.length() > longest) {
                longest = optString.length();
            }
            optStrings.add(optString.toString());
            optDescs.add(opt.getDescription());
        }
        final String format = " %1$-" + longest + "s  %2$s\n";
        for (int i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)));
        }
        return sb.toString();
    }
}
