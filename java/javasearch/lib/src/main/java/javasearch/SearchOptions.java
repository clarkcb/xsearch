/*******************************************************************************
SearchOptions

Class to encapsulate all command line search options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.*;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import static javafind.FindError.STARTPATH_NOT_DEFINED;

public class SearchOptions {
    private static final String SEARCH_OPTIONS_JSON_PATH = "/searchoptions.json";
    private final List<SearchOption> options;
    private final ArgTokenizer argTokenizer;

    public SearchOptions() throws IOException {
        options = new ArrayList<>();
        setOptionsFromJson();
        argTokenizer = new ArgTokenizer(options);
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, SearchSettings settings);
    }

    private final int boolActionMapSize = 31;
    private final Map<String, BooleanSetter> boolActionMap = new HashMap<>(boolActionMapSize) {
        {
            put("archivesonly", (b, settings) -> settings.setArchivesOnly(b));
            put("allmatches", (b, settings) -> settings.setFirstMatch(!b));
            put("colorize", (b, settings) -> settings.setColorize(b));
            put("debug", (b, settings) -> settings.setDebug(b));
            put("excludehidden", (b, settings) -> settings.setIncludeHidden(!b));
            put("firstmatch", (b, settings) -> settings.setFirstMatch(b));
            put("followsymlinks", (b, settings) -> settings.setFollowSymlinks(b));
            put("help", (b, settings) -> settings.setPrintUsage(b));
            put("includehidden", (b, settings) -> settings.setIncludeHidden(b));
            put("multilinesearch", (b, settings) -> settings.setMultiLineSearch(b));
            put("nocolorize", (b, settings) -> settings.setColorize(!b));
            put("nofollowsymlinks", (b, settings) -> settings.setFollowSymlinks(!b));
            put("noprintdirs", (b, settings) -> settings.setPrintDirs(!b));
            put("noprintfiles", (b, settings) -> settings.setPrintFiles(!b));
            put("noprintlines", (b, settings) -> settings.setPrintLines(!b));
            put("noprintmatches", (b, settings) -> settings.setPrintMatches(!b));
            put("noprintresults", (b, settings) -> settings.setPrintResults(!b));
            put("norecursive", (b, settings) -> settings.setRecursive(!b));
            put("nosearcharchives", (b, settings) -> settings.setSearchArchives(!b));
            put("printdirs", (b, settings) -> settings.setPrintDirs(b));
            put("printfiles", (b, settings) -> settings.setPrintFiles(b));
            put("printlines", (b, settings) -> settings.setPrintLines(b));
            put("printmatches", (b, settings) -> settings.setPrintMatches(b));
            put("printresults", (b, settings) -> settings.setPrintResults(b));
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
            var shortArg = "";
            if (searchOptionObj.has("short")) {
                shortArg = searchOptionObj.getString("short");
            }
            var longArg = searchOptionObj.getString("long");
            var desc = searchOptionObj.getString("desc");
            var argType = ArgTokenType.UNKNOWN;
            if (boolActionMap.containsKey(longArg)) {
                argType = ArgTokenType.BOOL;
            } else if (stringActionMap.containsKey(longArg)) {
                argType = ArgTokenType.STR;
            } else if (intActionMap.containsKey(longArg)) {
                argType = ArgTokenType.INT;
            } else if (longActionMap.containsKey(longArg)) {
                argType = ArgTokenType.LONG;
            }
            options.add(new SearchOption(shortArg, longArg, desc, argType));
        }
    }

    private void applyArgTokenToSettings(final ArgToken argToken, SearchSettings settings)
            throws SearchException {
        if (argToken.type().equals(ArgTokenType.BOOL)) {
            if (argToken.value() instanceof Boolean b) {
                this.boolActionMap.get(argToken.name()).set(b, settings);
            } else {
                throw new SearchException("Invalid value for option: " + argToken.name());
            }
        } else if (argToken.type().equals(ArgTokenType.STR)) {
            if (argToken.value() instanceof String s) {
                if (argToken.name().equals("settings-file")) {
                    updateSettingsFromFilePath(settings, s);
                } else {
                    this.stringActionMap.get(argToken.name()).set(s, settings);
                }
            } else if (argToken.value() instanceof Collection<?> coll) {
                for (Object item : coll) {
                    if (item instanceof String s) {
                        this.stringActionMap.get(argToken.name()).set(s, settings);
                    } else {
                        throw new SearchException("Invalid value for option: " + argToken.name());
                    }
                }
            } else if (argToken.value() instanceof JSONArray jsonArray) {
                for (var i = 0; i < jsonArray.length(); i++) {
                    Object item = jsonArray.get(i);
                    if (item instanceof String s) {
                        this.stringActionMap.get(argToken.name()).set(s, settings);
                    } else {
                        throw new SearchException("Invalid value for option: " + argToken.name());
                    }
                }
            } else {
                throw new SearchException("Invalid value for option: " + argToken.name());
            }
        } else if (argToken.type().equals(ArgTokenType.INT)) {
            if (argToken.value() instanceof Integer i) {
                this.intActionMap.get(argToken.name()).set(i, settings);
            } else if (argToken.value() instanceof Long l) {
                this.intActionMap.get(argToken.name()).set(l.intValue(), settings);
            } else {
                throw new SearchException("Invalid value for option: " + argToken.name());
            }
        } else if (argToken.type().equals(ArgTokenType.LONG)) {
            if (argToken.value() instanceof Integer i) {
                this.longActionMap.get(argToken.name()).set(i.longValue(), settings);
            } else if (argToken.value() instanceof Long l) {
                this.longActionMap.get(argToken.name()).set(l, settings);
            } else {
                throw new SearchException("Invalid value for option: " + argToken.name());
            }
        } else {
            throw new SearchException("Invalid option: " + argToken.name());
        }
    }

    private void updateSettingsFromArgTokens(SearchSettings settings, final List<ArgToken> argTokens) throws SearchException {
        for (var argToken : argTokens) {
            applyArgTokenToSettings(argToken, settings);
        }
    }

    public void updateSettingsFromJson(SearchSettings settings, final String json) throws SearchException {
        try {
            var argTokens = argTokenizer.tokenizeJson(json);
            updateSettingsFromArgTokens(settings, argTokens);
        } catch (FindException e) {
            throw new SearchException(e.getMessage());
        }
    }

    public final SearchSettings settingsFromJson(final String json) throws SearchException {
        var settings = new SearchSettings();
        updateSettingsFromJson(settings, json);
        return settings;
    }

    public void updateSettingsFromFilePath(SearchSettings settings, final String filePath) throws SearchException {
        try {
            var argTokens = argTokenizer.tokenizeFilePath(filePath);
            updateSettingsFromArgTokens(settings, argTokens);
        } catch (FindException e) {
            throw new SearchException(e.getMessage());
        }
    }

    public SearchSettings settingsFromFilePath(final String filePath) throws SearchException {
        var settings = new SearchSettings();
        updateSettingsFromFilePath(settings, filePath);
        return settings;
    }

    public final void updateSettingsFromArgs(SearchSettings settings, final String[] args) throws SearchException {
        try {
            var argTokens = argTokenizer.tokenizeArgs(args);
            updateSettingsFromArgTokens(settings, argTokens);
        } catch (FindException e) {
            throw new SearchException(e.getMessage());
        }
    }

    public final SearchSettings settingsFromArgs(final String[] args) throws SearchException {
        if (args == null || args.length == 0) {
            throw new SearchException(STARTPATH_NOT_DEFINED.getMessage());
        }

        var settings = new SearchSettings();
        // default printResults to true since running from command line
        settings.setPrintResults(true);
        updateSettingsFromArgs(settings, args);
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
            String shortArg = opt.shortArg();
            if (null != shortArg && !shortArg.isEmpty()) {
                optString.append("-").append(shortArg).append(",");
            }
            optString.append("--").append(opt.longArg());
            if (optString.length() > longest) {
                longest = optString.length();
            }
            optStrings.add(optString.toString());
            optDescs.add(opt.description());
        }
        final String format = " %1$-" + longest + "s  %2$s\n";
        for (int i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)));
        }
        return sb.toString();
    }
}
