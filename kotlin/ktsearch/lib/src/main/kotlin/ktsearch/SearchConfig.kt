package ktsearch

import ktfind.FindConfig
import java.nio.file.Paths

class SearchConfig : FindConfig() {
    val defaultXSearchConfigDir = Paths.get(System.getProperty("user.home"), ".config", "xsearch").toString()
    val searchOptionsPath: String
    val defaultSearchSettingsPath: String

    init {
        searchOptionsPath = "/searchoptions.json"
        val xSearchConfigDir = System.getenv("XSEARCH_CONFIG_DIR") ?: defaultXSearchConfigDir
        defaultSearchSettingsPath = Paths.get(xSearchConfigDir, "settings.json").toString()
    }
}
