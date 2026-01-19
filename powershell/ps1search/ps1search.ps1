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

    $colorize = $true
    $options = [SearchOptions]::new()

    try {
        $settings = $options.SettingsFromArgs($_args)
        $colorize = $settings.Colorize

        if ($settings.Debug) {
            # Set-LogConfiguration -LogLevel Debug
            LogMsg($settings.ToString())
        }

        if ($settings.PrintUsage) {
            LogMsg($options.GetUsageString())
            exit
        }

        $searcher = [Searcher]::new($settings)
        $results = $searcher.Search()
        $formatter = [SearchResultFormatter]::new($settings)

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

         if ($settings.PrintMatches) {
             $searcher.PrintMatches(@($results), $formatter)
         }
    }
    catch {
        $errMsg = $_.Exception.Message
        if ($colorize) {
            LogErrorColor($errMsg)
        } else {
            LogError($errMsg)
        }
        LogMsg($options.GetUsageString())
    }
}

SearchMain($args)
