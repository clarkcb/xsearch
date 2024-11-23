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

        if ($settings.PrintResults) {
            $searcher.PrintResults(@($results))
        }

         if ($settings.PrintDirs) {
             $searcher.PrintMatchingDirs(@($results))
         }

         if ($settings.PrintFiles) {
             $searcher.PrintMatchingFiles(@($results))
         }

         if ($settings.PrintLines) {
             $searcher.PrintMatchingLines(@($results), $settings)
         }
    }
    catch {
        LogError($_)
        LogMsg($options.GetUsageString())
    }
}

SearchMain($args)
