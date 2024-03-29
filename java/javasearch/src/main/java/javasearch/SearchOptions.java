/*******************************************************************************
SearchOptions

Class to encapsulate all command line search options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.FileUtil;
import javafind.Logger;
import javafind.SortBy;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class SearchOptions {
    private static final String SEARCH_OPTIONS_JSON_PATH = "/searchoptions.json";
    private final List<SearchOption> options;

    public SearchOptions() throws IOException {
        options = new ArrayList<>();
        setOptionsFromJson();
    }

    @FunctionalInterface
    private interface ArgSetter {
        void set(String s, SearchSettings settings);
    }

    private final int actionMapSize = 32;
    private final Map<String, ArgSetter> argActionMap = new HashMap<>(actionMapSize) {
        {
            put("encoding", (s, settings) -> settings.setTextFileEncoding(s));
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
            put("maxdepth", (s, settings) -> settings.setMaxDepth(Integer.parseInt(s)));
            put("maxlastmod", (s, settings) -> settings.setMaxLastMod(s));
            put("maxlinelength", (s, settings) -> settings.setMaxLineLength(Integer.parseInt(s)));
            put("maxsize", (s, settings) -> settings.setMaxSize(Integer.parseInt(s)));
            put("mindepth", (s, settings) -> settings.setMinDepth(Integer.parseInt(s)));
            put("minlastmod", (s, settings) -> settings.setMinLastMod(s));
            put("minsize", (s, settings) -> settings.setMinSize(Integer.parseInt(s)));
            put("out-archiveext", (s, settings) -> settings.addOutArchiveExtension(s));
            put("out-archivefilepattern", (s, settings) -> settings.addOutArchiveFilePattern(s));
            put("out-dirpattern", (s, settings) -> settings.addOutDirPattern(s));
            put("out-ext", (s, settings) -> settings.addOutExtension(s));
            put("out-filepattern", (s, settings) -> settings.addOutFilePattern(s));
            put("out-filetype", (s, settings) -> settings.addOutFileType(s));
            put("out-linesafterpattern", (s, settings) -> settings.addOutLinesAfterPattern(s));
            put("out-linesbeforepattern", (s, settings) -> settings.addOutLinesBeforePattern(s));
            put("path", (s, settings) -> settings.addPath(s));
            put("searchpattern", (s, settings) -> settings.addSearchPattern(s));
            put("settings-file", (s, settings) -> settingsFromFilePath(s, settings));
            put("sort-by", (s, settings) -> settings.setSortBy(SortBy.forName(s)));
        }
    };

    @FunctionalInterface
    private interface BooleanFlagSetter {
        void set(Boolean b, SearchSettings settings);
    }

    private final int flagMapSize = 29;
    private final Map<String, BooleanFlagSetter> boolflagActionMap = new HashMap<>(flagMapSize) {
        {
            put("archivesonly", (b, settings) -> settings.setArchivesOnly(b));
            put("allmatches", (b, settings) -> settings.setFirstMatch(!b));
            put("colorize", (b, settings) -> settings.setColorize(b));
            put("debug", (b, settings) -> settings.setDebug(b));
            put("excludehidden", (b, settings) -> settings.setIncludeHidden(!b));
            put("firstmatch", (b, settings) -> settings.setFirstMatch(b));
            put("help", (b, settings) -> settings.setPrintUsage(b));
            put("includehidden", (b, settings) -> settings.setIncludeHidden(b));
            put("multilinesearch", (b, settings) -> settings.setMultiLineSearch(b));
            put("nocolorize", (b, settings) -> settings.setColorize(!b));
            put("noprintdirs", (b, settings) -> settings.setPrintDirs(!b));
            put("noprintfiles", (b, settings) -> settings.setPrintFiles(!b));
            put("noprintlines", (b, settings) -> settings.setPrintLines(!b));
            put("noprintmatches", (b, settings) -> settings.setPrintResults(!b));
            put("norecursive", (b, settings) -> settings.setRecursive(!b));
            put("nosearcharchives", (b, settings) -> settings.setSearchArchives(!b));
            put("printdirs", (b, settings) -> settings.setPrintDirs(b));
            put("printfiles", (b, settings) -> settings.setPrintFiles(b));
            put("printlines", (b, settings) -> settings.setPrintLines(b));
            put("printmatches", (b, settings) -> settings.setPrintResults(b));
            put("recursive", (b, settings) -> settings.setRecursive(b));
            put("searcharchives", (b, settings) -> settings.setSearchArchives(b));
            put("sort-ascending", (b, settings) -> settings.setSortDescending(!b));
            put("sort-caseinsensitive", (b, settings) -> settings.setSortCaseInsensitive(b));
            put("sort-casesensitive", (b, settings) -> settings.setSortCaseInsensitive(!b));
            put("sort-descending", (b, settings) -> settings.setSortDescending(b));
            put("uniquelines", (b, settings) -> settings.setUniqueLines(b));
            put("verbose", (b, settings) -> settings.setVerbose(b));
            put("version", (b, settings) -> settings.setPrintVersion(b));
        }
    };

    private void setOptionsFromJson() {
        InputStream searchOptionsInputStream = getClass().getResourceAsStream(SEARCH_OPTIONS_JSON_PATH);
        assert searchOptionsInputStream != null;
        var jsonObj = new JSONObject(new JSONTokener(searchOptionsInputStream));
        var searchOptionsArray = jsonObj.getJSONArray("searchoptions");

        for (int i=0; i<searchOptionsArray.length(); i++) {
            var searchOptionObj = searchOptionsArray.getJSONObject(i);
            var longArg = searchOptionObj.getString("long");
            var desc = searchOptionObj.getString("desc");
            var shortArg = "";
            if (searchOptionObj.has("short")) {
                shortArg = searchOptionObj.getString("short");
            }
            options.add(new SearchOption(shortArg, longArg, desc));
        }
    }

    private void settingsFromFilePath(final String filePath, final SearchSettings settings) {
        var path = Paths.get(filePath);
        try {
            if (Files.exists(path)) {
                Logger.log("Settings file not found: " + filePath);
                System.exit(1);
            }
            if (!FileUtil.hasExtension(filePath, "json")) {
                Logger.log("Invalid settings file type (just be JSON): " + filePath);
                System.exit(1);
            }
            settingsFromJson(FileUtil.getFileContents(path), settings);
        } catch (FileNotFoundException e) {
            Logger.log("Settings file not found: " + filePath);
            System.exit(1);
        } catch (IOException e) {
            Logger.log("IOException reading settings file: " + filePath);
            System.exit(1);
        }
    }

    public void settingsFromJson(final String json, SearchSettings settings) {
        var jsonObj = new JSONObject(new JSONTokener(json));
        for (var ko : jsonObj.keySet()) {
            var vo = jsonObj.get(ko);
            applySetting(ko, vo, settings);
        }
    }

    private void applySetting(final String arg, final Object obj, SearchSettings settings) {
        if (obj instanceof String) {
            try {
                applySetting(arg, (String)obj, settings);
            } catch (SearchException e) {
                Logger.logError("SearchException: " + e.getMessage());
            }
        } else if (obj instanceof Boolean) {
            try {
                applySetting(arg, (Boolean)obj, settings);
            } catch (SearchException e) {
                Logger.logError("SearchException: " + e.getMessage());
            }
        } else if ((obj instanceof Integer) || (obj instanceof Long)) {
            try {
                applySetting(arg, obj.toString(), settings);
            } catch (SearchException e) {
                Logger.logError("SearchException: " + e.getMessage());
            }
        } else if (obj instanceof JSONArray) {
            for (int i=0; i < ((JSONArray)obj).length(); i++) {
                applySetting(arg, ((JSONArray)obj).get(i), settings);
            }
        } else {
            Logger.log("obj is another class type");
        }
    }

    private void applySetting(final String arg, final String val, SearchSettings settings)
            throws SearchException{
        if (this.argActionMap.containsKey(arg)) {
            this.argActionMap.get(arg).set(val, settings);
        } else if (arg.equals("path")) {
            settings.addPath(val);
        } else {
            throw new SearchException("Invalid option: " + arg);
        }
    }

    private void applySetting(final String arg, final Boolean val, SearchSettings settings)
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
                while (arg.startsWith("-")) {
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
                settings.addPath(arg);
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
        sb.append(" javasearch [options] -s <searchpattern> <path> [<path> ...]\n\n");
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
