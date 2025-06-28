#!/usr/bin/env pwsh
################################################################################
#
# ps1search.ps1
#
# A powershell version of xsearch
#
# TODO: install module under $env:PSModulePath
#
################################################################################
using module 'Ps1SearchModule'

function SearchMain {
    param(
        [string[]]$_args
    )

    [SearchOptions]$options = [SearchOptions]::new()

    try {
        [SearchSettings]$settings = $options.SettingsFromArgs($_args)

        if ($settings.Debug) {
            LogMsg($settings.ToString())
        }

        if ($settings.PrintUsage) {
            LogMsg($options.GetUsageString())
            exit
        }

        [Searcher]$searcher = [Searcher]::new($settings)
        [SearchResult[]]$results = $searcher.Search()
        [SearchResultFormatter]$formatter = [SearchResultFormatter]::new($settings)

        if ($settings.PrintResults) {
            $searcher.PrintResults($results, $formatter)
        }

         if ($settings.PrintDirs) {
             $searcher.PrintMatchingDirs(@($results), $formatter)
         }

         if ($settings.PrintFiles) {
             $searcher.PrintMatchingFiles(@($results), $formatter)
         }

         if ($settings.PrintLines) {
             $searcher.PrintMatchingLines(@($results), $formatter)
         }
    }
    catch {
        LogError($_)
        LogMsg($options.GetUsageString())
    }
}

SearchMain($args)
