/*******************************************************************************
SearchOptions

Class to encapsulate all command line search options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.*;
import java.util.*;

public class SearchOptions {
    private final List<SearchOption> options;

    public SearchOptions() throws IOException, ParseException {
        options = new ArrayList<>();
        setOptionsFromJson();
    }

    @FunctionalInterface
    private interface ArgSetter {
        void set(String s, SearchSettings settings);
    }

    private final int actionMapSize = 24;
    private final Map<String, ArgSetter> argActionMap = new HashMap<String, ArgSetter>(actionMapSize) {
        {
            put("in-archiveext", (s, settings) -> settings.addInArchiveExtension(s));
            put("in-archivefilepattern", (s, settings) -> settings.addInArchiveFilePattern(s));
            put("in-dirpattern", (s, settings) -> settings.addInDirPattern(s));
            put("in-ext", (s, settings) -> settings.addInExtension(s));
            put("in-filepattern", (s, settings) -> settings.addInFilePattern(s));
            put("in-filetype", (s, settings) -> settings.addInFileType(s));
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
            put("out-filetype", (s, settings) -> settings.addOutFileType(s));
            put("out-linesafterpattern", (s, settings) -> settings.addOutLinesAfterPattern(s));
            put("out-linesbeforepattern", (s, settings) -> settings.addOutLinesBeforePattern(s));
            put("searchpattern", (s, settings) -> settings.addSearchPattern(s));
            put("settings-file", (s, settings) -> settingsFromFile(s, settings));
        }
    };

    @FunctionalInterface
    private interface BooleanFlagSetter {
        void set(Boolean b, SearchSettings settings);
    }

    private final int flagMapSize = 22;
    private final Map<String, BooleanFlagSetter> boolflagActionMap = new HashMap<String, BooleanFlagSetter>(flagMapSize) {
        {
            put("archivesonly", (b, settings) -> settings.setArchivesOnly(b));
            put("allmatches", (b, settings) -> settings.setFirstMatch(!b));
            put("colorize", (b, settings) -> settings.setColorize(b));
            put("debug", (b, settings) -> settings.setDebug(b));
            put("excludehidden", (b, settings) -> settings.setExcludeHidden(b));
            put("firstmatch", (b, settings) -> settings.setFirstMatch(b));
            put("help", (b, settings) -> settings.setPrintUsage(b));
            put("includehidden", (b, settings) -> settings.setExcludeHidden(!b));
            put("listdirs", (b, settings) -> settings.setListDirs(b));
            put("listfiles", (b, settings) -> settings.setListFiles(b));
            put("listlines", (b, settings) -> settings.setListLines(b));
            put("multilinesearch", (b, settings) -> settings.setMultiLineSearch(b));
            put("nocolorize", (b, settings) -> settings.setColorize(!b));
            put("noprintmatches", (b, settings) -> settings.setPrintResults(!b));
            put("norecursive", (b, settings) -> settings.setRecursive(!b));
            put("nosearcharchives", (b, settings) -> settings.setSearchArchives(!b));
            put("printmatches", (b, settings) -> settings.setPrintResults(b));
            put("recursive", (b, settings) -> settings.setRecursive(b));
            put("searcharchives", (b, settings) -> settings.setSearchArchives(b));
            put("uniquelines", (b, settings) -> settings.setUniqueLines(b));
            put("verbose", (b, settings) -> settings.setVerbose(b));
            put("version", (b, settings) -> settings.setPrintVersion(b));
        }
    };

    private void setOptionsFromJson() throws IOException, ParseException {
        final String searchOptionsJsonPath = "/searchoptions.json";
        InputStream searchOptionsInputStream = getClass().getResourceAsStream(searchOptionsJsonPath);
        Object obj = new JSONParser().parse(new InputStreamReader(searchOptionsInputStream));
        JSONObject jsonObj = (JSONObject)obj;
        JSONArray searchoptionsArray = (JSONArray) jsonObj.get("searchoptions");

        for (Object o : searchoptionsArray) {
            Map searchoptionMap = (Map) o;
            String longArg = (String) searchoptionMap.get("long");
            String desc = (String) searchoptionMap.get("desc");
            String shortArg = "";
            if (searchoptionMap.containsKey("short")) {
                shortArg = (String) searchoptionMap.get("short");
            }
            options.add(new SearchOption(shortArg, longArg, desc));
        }
    }

    private List<String> listFromJSONArray(JSONArray arr) {
        List<String> list = new ArrayList<>();
        for (Object o : arr) {
            list.add((String)o);
        }
        return list;
    }

    private void settingsFromFile(String filePath, SearchSettings settings) {
        File file = new File(filePath);
        try {
            if (!file.exists()) {
                Logger.log("Settings file not found: " + filePath);
                System.exit(1);
            }
            if (!FileUtil.hasExtension(filePath, "json")) {
                Logger.log("Invalid settings file type (just be JSON): " + filePath);
                System.exit(1);
            }
            settingsFromJson(FileUtil.getFileContents(file), settings);
        } catch (FileNotFoundException e) {
            Logger.log("Settings file not found: " + filePath);
            System.exit(1);
        } catch (IOException e) {
            Logger.log("IOException reading settings file: " + filePath);
            System.exit(1);
        } catch (ParseException e) {
            Logger.log("ParseException trying to parse the JSON in " + filePath);
            System.exit(1);
        }
    }

    public void settingsFromJson(String json, SearchSettings settings) throws ParseException {
        Object obj = JSONValue.parseWithException(json);
        JSONObject jsonObject=(JSONObject)obj;
        for (Object ko : jsonObject.keySet()) {
            String k = (String)ko;
            Object vo =jsonObject.get(ko);
            applySetting(k, vo, settings);
        }
    }

    private void applySetting(String arg, Object obj, SearchSettings settings) {
        if (obj.getClass().equals(String.class)) {
            try {
                applySetting(arg, (String)obj, settings);
            } catch (SearchException e) {
                Logger.logError("SearchException: " + e.getMessage());
            }
        } else if (obj.getClass().equals(Boolean.class)) {
            try {
                applySetting(arg, (Boolean)obj, settings);
            } catch (SearchException e) {
                Logger.logError("SearchException: " + e.getMessage());
            }
        } else if (obj.getClass().equals(Long.class)) {
            try {
                applySetting(arg, obj.toString(), settings);
            } catch (SearchException e) {
                Logger.logError("SearchException: " + e.getMessage());
            }
        } else if (obj.getClass().equals(JSONArray.class)) {
            for (String s : listFromJSONArray((JSONArray)obj)) {
                try {
                    applySetting(arg, s, settings);
                } catch (SearchException e) {
                    Logger.logError("SearchException: " + e.getMessage());
                }
            }
        } else {
            Logger.log("obj is another class type");
        }
    }

    private void applySetting(String arg, String val, SearchSettings settings)
            throws SearchException{
        if (this.argActionMap.containsKey(arg)) {
            this.argActionMap.get(arg).set(val, settings);
        } else if (arg.equals("startpath")) {
            settings.setStartPath(val);
        } else {
            throw new SearchException("Invalid option: " + arg);
        }
    }

    private void applySetting(String arg, Boolean val, SearchSettings settings)
            throws SearchException{
        if (this.boolflagActionMap.containsKey(arg)) {
            this.boolflagActionMap.get(arg).set(val, settings);
        } else {
            throw new SearchException("Invalid option: " + arg);
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
            } else if (boolflagActionMap.containsKey(o.getLongArg())) {
                boolflagActionMap.put(o.getShortArg(), boolflagActionMap.get(o.getLongArg()));
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
                } else if (this.boolflagActionMap.containsKey(arg)) {
                    this.boolflagActionMap.get(arg).set(true, settings);
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

        this.options.sort(Comparator.comparing(SearchOption::getSortArg));

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
