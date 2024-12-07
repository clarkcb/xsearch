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
    private final Map<String, String> longArgMap = new HashMap<>();

    public SearchOptions() throws IOException {
        options = new ArrayList<>();
        setOptionsFromJson();
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, SearchSettings settings);
    }

    private final int boolActionMapSize = 29;
    private final Map<String, BooleanSetter> boolActionMap = new HashMap<>(boolActionMapSize) {
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

    @FunctionalInterface
    private interface StringSetter {
        void set(String s, SearchSettings settings);
    }

    private final int stringActionMapSize = 25;
    private final Map<String, StringSetter> stringActionMap = new HashMap<>(stringActionMapSize) {
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
            put("linesaftertopattern", (s, settings) -> settings.addLinesAfterToPattern(s));
            put("linesafteruntilpattern", (s, settings) -> settings.addLinesAfterUntilPattern(s));
            put("maxlastmod", (s, settings) -> settings.setMaxLastMod(s));
            put("minlastmod", (s, settings) -> settings.setMinLastMod(s));
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
            put("sort-by", (s, settings) -> settings.setSortBy(SortBy.forName(s)));
        }
    };

    @FunctionalInterface
    private interface IntegerSetter {
        void set(Integer i, SearchSettings settings);
    }

    private final int intActionMapSize = 5;
    private final Map<String, IntegerSetter> intActionMap = new HashMap<>(intActionMapSize) {
        {
            put("linesafter", (i, settings) -> settings.setLinesAfter(i));
            put("linesbefore", (i, settings) -> settings.setLinesBefore(i));
            put("maxdepth", (i, settings) -> settings.setMaxDepth(i));
            put("maxlinelength", (i, settings) -> settings.setMaxLineLength(i));
            put("mindepth", (i, settings) -> settings.setMinDepth(i));
        }
    };

    @FunctionalInterface
    private interface LongSetter {
        void set(Long l, SearchSettings settings);
    }

    private final int longActionMapSize = 2;
    private final Map<String, LongSetter> longActionMap = new HashMap<>(longActionMapSize) {
        {
            put("maxsize", (l, settings) -> settings.setMaxSize(l));
            put("minsize", (l, settings) -> settings.setMinSize(l));
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
            longArgMap.put(longArg, longArg);
            var desc = searchOptionObj.getString("desc");
            var shortArg = "";
            if (searchOptionObj.has("short")) {
                shortArg = searchOptionObj.getString("short");
                longArgMap.put(shortArg, longArg);
            }
            options.add(new SearchOption(shortArg, longArg, desc));
        }
    }

    private void settingsFromFilePath(final String filePath, final SearchSettings settings) throws SearchException {
        var path = Paths.get(filePath);
        try {
            if (!Files.exists(path)) {
                throw new SearchException("Settings file not found: " + filePath);
            }
            if (!FileUtil.hasExtension(filePath, "json")) {
                throw new SearchException("Invalid settings file type (must be JSON): " + filePath);
            }
            settingsFromJson(FileUtil.getFileContents(path), settings);
        } catch (FileNotFoundException e) {
            throw new SearchException("Settings file not found: " + filePath);
        } catch (IOException e) {
            throw new SearchException("IOException reading settings file: " + filePath);
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
        if (this.stringActionMap.containsKey(arg)) {
            this.stringActionMap.get(arg).set(val, settings);
        } else if (arg.equals("path")) {
            settings.addPath(val);
        } else {
            throw new SearchException("Invalid option: " + arg);
        }
    }

    private void applySetting(final String arg, final Boolean val, SearchSettings settings)
            throws SearchException{
        if (this.boolActionMap.containsKey(arg)) {
            this.boolActionMap.get(arg).set(val, settings);
        } else {
            throw new SearchException("Invalid option: " + arg);
        }
    }

    public final SearchSettings settingsFromArgs(final String[] args) throws SearchException {
        SearchSettings settings = new SearchSettings();
        // default printResults to true since running from command line
        settings.setPrintResults(true);

        Queue<String> queue = new LinkedList<>(Arrays.asList(args));
        while (!queue.isEmpty()) {
            String arg = queue.remove();
            if (arg.startsWith("-")) {
                while (arg.startsWith("-")) {
                    arg = arg.substring(1);
                }
                var longArg = longArgMap.get(arg);
                if (this.boolActionMap.containsKey(longArg)) {
                    this.boolActionMap.get(longArg).set(true, settings);
                } else if (this.stringActionMap.containsKey(longArg)
                        || this.intActionMap.containsKey(longArg)
                        || this.longActionMap.containsKey(longArg)
                        || longArg.equals("settings-file")) {
                    if (!queue.isEmpty()) {
                        String argVal = queue.remove();
                        if (this.stringActionMap.containsKey(longArg)) {
                            this.stringActionMap.get(longArg).set(argVal, settings);
                        } else if (this.intActionMap.containsKey(longArg)) {
                            this.intActionMap.get(longArg).set(Integer.parseInt(argVal), settings);
                        } else if (this.longActionMap.containsKey(longArg)) {
                            this.longActionMap.get(longArg).set(Long.parseLong(argVal), settings);
                        } else if (longArg.equals("settings-file")) {
                            settingsFromFilePath(argVal, settings);
                        }
                    } else {
                        throw new SearchException("Missing value for option " + arg);
                    }
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
