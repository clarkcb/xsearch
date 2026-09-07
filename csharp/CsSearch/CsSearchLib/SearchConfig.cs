using System;
using System.IO;
using CsFindLib;

namespace CsSearchLib;

public class SearchConfig : FindConfig
{
    public string SearchOptionsPath { get; private set; }
    public string DefaultSearchSettingsPath { get; private set; }

    public SearchConfig()
    {
        SearchOptionsPath = "CsSearchLib.Resources.searchoptions.json";

        var homePath = FileUtil.GetHomePath();
        var defaultXSearchConfigDir = Path.Join(homePath, ".config", "xsearch");
        var xSearchConfigDir = Environment.GetEnvironmentVariable("XSEARCH_CONFIG_DIR");
        if (string.IsNullOrEmpty(xSearchConfigDir))
        {
            xSearchConfigDir = defaultXSearchConfigDir;
        }

        DefaultSearchSettingsPath = Path.Join(xSearchConfigDir, "settings.json");
    }
}
