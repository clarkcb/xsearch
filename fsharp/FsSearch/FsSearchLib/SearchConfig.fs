namespace FsSearchLib
open System
open System.IO
open FsFindLib

type SearchConfig() =
    inherit FindConfig()

    let getXSearchConfigDir () =
        let defaultXSearchConfigDir = Path.Join(FileUtil.GetHomePath(), ".config", "xsearch")
        let xSearchConfigDir = Environment.GetEnvironmentVariable("XSEARCH_CONFIG_DIR")
        if String.IsNullOrEmpty(xSearchConfigDir) then
            defaultXSearchConfigDir
        else
            xSearchConfigDir

    member val SearchOptionsPath : string = "FsSearchLib.Resources.searchoptions.json" with get, set
    member val DefaultSearchSettingsPath : string = Path.Join(getXSearchConfigDir(), "settings.json") with get, set
