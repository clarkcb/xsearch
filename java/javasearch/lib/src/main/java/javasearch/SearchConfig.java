/*******************************************************************************
SearchConfig

Class to encapsulate a command line search configuration

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2026
*******************************************************************************/

package javasearch;

import javafind.FindConfig;

import java.nio.file.Paths;

public class SearchConfig extends FindConfig {
    public static final String DEFAULT_SEARCH_OPTIONS_PATH = "/searchoptions.json";
    public static final String DEFAULT_XSEARCH_CONFIG_DIR =
            Paths.get(System.getProperty("user.home"), ".config", "xsearch").toString();

    private final String searchOptionsPath;
    private final String defaultSearchSettingsPath;

    public SearchConfig() {
        this.searchOptionsPath = DEFAULT_SEARCH_OPTIONS_PATH;
        var xSearchConfigDir = getXSearchConfigDir();
        this.defaultSearchSettingsPath = Paths.get(xSearchConfigDir, "settings.json").toString();
    }

    public String getSearchOptionsPath() {
        return searchOptionsPath;
    }

    public String getXSearchConfigDir() {
        var xsearchConfigDir = System.getenv("XSEARCH_CONFIG_DIR");
        if (xsearchConfigDir == null || xsearchConfigDir.isEmpty()) {
            xsearchConfigDir = DEFAULT_XSEARCH_CONFIG_DIR;
        }
        return xsearchConfigDir;
    }

    public String getDefaultSearchSettingsPath() {
        return defaultSearchSettingsPath;
    }
}
