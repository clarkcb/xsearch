# https://dev.to/omiossec/unit-testing-in-powershell-introduction-to-pester-1de7

using module 'Ps1FindModule'
using module 'Ps1SearchModule'

#region SearchSettings
Describe -tag "SearchSettings" -name "test_default_settings" {
    It "has valid default settings" {
        $settings = [SearchSettings]::new()

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Colorize | Should -BeTrue
        $settings.Debug | Should -BeFalse
        $settings.FirstMatch | Should -BeFalse
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.PrintDirs | Should -BeFalse
        $settings.PrintFiles | Should -BeFalse
        $settings.PrintLines | Should -BeFalse
        $settings.PrintResults | Should -BeFalse
        $settings.PrintUsage | Should -BeFalse
        $settings.Recursive | Should -BeTrue
        $settings.SearchArchives | Should -BeFalse
        $settings.Verbose | Should -BeFalse
    }
}

Describe -tag "SearchSettings" -name "test_add_single_extension" {
    It "has one extension" {
        $settings = [SearchSettings]::new()
        $settings.InExtensions += $settings.GetExtensions('ps1')
        $settings.InExtensions.Count | Should -BeExactly 1
    }
}

Describe -tag "SearchSettings" -name "test_add_comma_delimited_extensions" {
    It "has two extensions" {
        $settings = [SearchSettings]::new()
        $settings.InExtensions += $settings.GetExtensions('ps1,php')
        $settings.InExtensions.Count | Should -BeExactly 2
    }
}

Describe -tag "SearchSettings" -name "test_add_extensions_array" {
    It "has two extensions" {
        $settings = [SearchSettings]::new()
        $settings.InExtensions += @('ps1' ,'php')
        $settings.InExtensions.Count | Should -BeExactly 2
    }
}

Describe -tag "SearchSettings" -name "test_add_pattern" {
    It "has two extensions" {
        $settings = [SearchSettings]::new()
        $settings.InFilePatterns += [regex]"search"
        $settings.InFilePatterns.Count | Should -BeExactly 1
    }
}

Describe -tag "SearchSettings" -name "test_add_patterns_array" {
    It "has two extensions" {
        $settings = [SearchSettings]::new()
        $settings.InFilePatterns += @([regex]"search", [regex]"file")
        $settings.InFilePatterns.Count | Should -BeExactly 2
    }
}

Describe -tag "SearchSettings" -name "test_set_archives_only" {
    It "archives_only and include_archives are true" {
        $settings = [SearchSettings]::new()
        $settings.SetArchivesOnly($true)
        $settings.ArchivesOnly | Should -BeTrue
        $settings.IncludeArchives | Should -BeTrue
    }
}

Describe -tag "SearchSettings" -name "test_set_debug" {
    It "debug and verbose are true" {
        $settings = [SearchSettings]::new()
        $settings.SetDebug($true)
        $settings.Debug | Should -BeTrue
        $settings.Verbose | Should -BeTrue
    }
}
#endregion


#region SearchOptions
Describe -tag "SearchOptions" -name "test_settings_from_args_no_args" {
    It "equals default settings" {
        $options = [SearchOptions]::new()
        $_args = @()
        $settings = $options.SettingsFromArgs($_args)

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Colorize | Should -BeTrue
        $settings.Debug | Should -BeFalse
        $settings.FirstMatch | Should -BeFalse
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.InExtensions.Count | Should -BeExactly 0
        $settings.PrintDirs | Should -BeFalse
        $settings.PrintFiles | Should -BeFalse
        $settings.PrintLines | Should -BeFalse
        $settings.PrintResults | Should -BeTrue
        $settings.PrintUsage | Should -BeFalse
        $settings.Recursive | Should -BeTrue
        $settings.SearchArchives | Should -BeFalse
        $settings.SearchPatterns.Count | Should -BeExactly 0
        $settings.Verbose | Should -BeFalse
    }
}

Describe -tag "SearchOptions" -name "test_settings_from_args_valid_args" {
    It "has valid settings" {
        $options = [SearchOptions]::new()
        $_args = @('-x', 'php,py', '-s', 'Searcher', '.')
        $settings = $options.SettingsFromArgs($_args)

        $settings.ArchivesOnly | Should -BeFalse
        $settings.Colorize | Should -BeTrue
        $settings.Debug | Should -BeFalse
        $settings.FirstMatch | Should -BeFalse
        $settings.IncludeHidden | Should -BeFalse
        $settings.IncludeArchives | Should -BeFalse
        $settings.InExtensions.Count | Should -BeExactly 2
        $settings.InExtensions[0] | Should -BeExactly '.php'
        $settings.InExtensions[1] | Should -BeExactly '.py'
        $settings.PrintDirs | Should -BeFalse
        $settings.PrintFiles | Should -BeFalse
        $settings.PrintLines | Should -BeFalse
        $settings.PrintResults | Should -BeTrue
        $settings.PrintUsage | Should -BeFalse
        $settings.Recursive | Should -BeTrue
        $settings.SearchArchives | Should -BeFalse
        $settings.SearchPatterns.Count | Should -BeExactly 1
        $settings.SearchPatterns[0] | Should -BeExactly 'Searcher'
        $settings.Verbose | Should -BeFalse
    }
}

Describe -tag "SearchOptions" -name "test_settings_from_args_invalid_args" {
    It "reports invalid option" {
        $options = [SearchOptions]::new()
        $_args = @('-x', 'php,py', '-Q', '-s', 'Searcher', '.')
        try {
            $settings = $options.SettingsFromArgs($_args)
        } catch {
            $_.ToString() | Should -BeExactly "Invalid option: Q"
        }
    }
}
#endregion


#region SearchResult
Describe -tag "SearchResult" -name "test_search_result_abs_path" {
    It "matches result with absolute path" {
        # /Users/cary/src/xsearch/powershell/ps1search/ps1search.ps1: 32 [10:18]: [Searcher]$searcher = [Searcher]::new($settings)
        $path = "/home/user/src/xsearch/powershell/ps1search";
        $fileName = 'ps1search.ps1';
        $file = [System.IO.FileInfo]::new("$path/$fileName")
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $fileResult.File.ToString() | Should -BeExactly "/home/user/src/xsearch/powershell/ps1search/ps1search.ps1"
        $pattern = [regex]"Searcher"
        $lineNum = 32
        $matchStartIdx = 10
        $matchEndIdx = 18
        $line = "        [Searcher]`$searcher = [Searcher]::new(`$settings)"
        $searchResult = [SearchResult]::new($pattern, $fileResult, $lineNum, $matchStartIdx, $matchEndIdx, $line)
        $searchResult.SearchPattern | Should -BeExactly $pattern
        $searchResult.LineNum | Should -BeExactly $lineNum
        $searchResult.MatchStartIndex | Should -BeExactly $matchStartIdx
        $searchResult.MatchEndIndex | Should -BeExactly $matchEndIdx
        $searchResult.Line | Should -BeExactly $line
    }
}
#endregion


#region SearchResultFormatter
Describe -tag "SearchResultFormatter" -name "test_format_colorized_single_line_result" {
    It "matches colorized result" {
        # Create Settings
        $settings = [SearchSettings]::new()
        $settings.SearchPatterns += [regex]"Searcher"

        # Create SearchResultFormatter
        $formatter = [SearchResultFormatter]::new($settings)

        # Create SearchResult
        $filePath = "/home/user/src/xsearch/powershell/ps1search/ps1search.ps1";
        $file = [System.IO.FileInfo]::new("$filePath")
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $pattern = [regex]"Searcher"
        $lineNum = 32
        $matchStartIdx = 10
        $matchEndIdx = 18
        $line = "        [Searcher]`$searcher = [Searcher]::new(`$settings)"
        $searchResult = [SearchResult]::new($pattern, $fileResult, $lineNum, $matchStartIdx, $matchEndIdx, $line)

        $formattedResult = $formatter.Format($searchResult)
        $formattedResult.LinesBefore.Count | Should -BeExactly 0
        $formattedResult.MatchingLine.Count | Should -BeExactly 4
        $formattedResult.LinesAfter.Count | Should -BeExactly 0
    }
}

Describe -tag "SearchResultFormatter" -name "test_format_uncolorized_single_line_result" {
    It "matches uncolorized result" {
        # Create Settings
        $settings = [SearchSettings]::new()
        $settings.Colorize = $false
        $settings.SearchPatterns += [regex]"Searcher"

        # Create SearchResultFormatter
        $formatter = [SearchResultFormatter]::new($settings)

        # Create SearchResult
        $filePath = "/home/user/src/xsearch/powershell/ps1search/ps1search.ps1";
        $file = [System.IO.FileInfo]::new("$filePath")
        $fileResult = [FileResult]::new($file, [FileType]::Code);
        $pattern = [regex]"Searcher"
        $lineNum = 32
        $matchStartIdx = 10
        $matchEndIdx = 18
        $line = "        [Searcher]`$searcher = [Searcher]::new(`$settings)"
        $searchResult = [SearchResult]::new($pattern, $fileResult, $lineNum, $matchStartIdx, $matchEndIdx, $line)

        $formattedResult = $formatter.Format($searchResult)
        $formattedResult.LinesBefore.Count | Should -BeExactly 0
        $formattedResult.MatchingLine.Count | Should -BeExactly 2
        $formattedResult.LinesAfter.Count | Should -BeExactly 0
    }
}
#endregion


#region Searcher
Describe -tag "Searcher" -name "test_searcher_search_test_file" {
    It "matches test file expected results" {
        # Create Settings
        $settings = [SearchSettings]::new()
        $settings.InExtensions += $settings.GetExtensions('txt')
        $settings.InFilePatterns += [regex]"testFile2"
        $settings.SearchPatterns += [regex]"Searcher"
        $testFilesPath = Join-Path $env:XSEARCH_PATH 'shared' 'testFiles'
        $settings.Paths += $testFilesPath

        # Create Searcher
        $searcher = [Searcher]::new($settings)
        
        # Search and get results
        $results = $searcher.Search()
        
        $results.Count | Should -BeExactly 2
        $results[0].LineNum | Should -BeExactly 30
        $results[1].LineNum | Should -BeExactly 36
    }
}
#endregion
