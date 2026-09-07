package scalasearch

import scalafind.FindConfig

import java.nio.file.Paths

class SearchConfig extends FindConfig {
  val defaultXSearchConfigDir: String = Paths.get(System.getProperty("user.home"), ".config", "xsearch").toString
  val searchOptionsPath: String = "/searchoptions.json"
  val xSearchConfigDir: String = {
    if (System.getenv("XSEARCH_CONFIG_DIR") != null) {
      System.getenv("XSEARCH_CONFIG_DIR")
    } else {
      defaultXSearchConfigDir
    }
  }
  val defaultSearchSettingsPath: String = Paths.get(xSearchConfigDir, "settings.json").toString
}
