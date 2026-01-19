################################################################################
#
# Ps1SearchModule.psm1
#
# A single module file for ps1search
#
# install module under $env:PSModulePath
#
################################################################################
using module 'Ps1FindModule'


#region Config
########################################
# Config
########################################
if (-not (Test-Path env:XSEARCH_PATH)) {
    $env:XSEARCH_PATH = Join-Path -Path $HOME -ChildPath 'src' -AdditionalChildPath 'xsearch'
}
$xsearchPath = $env:XSEARCH_PATH
$sharedPath = Join-Path -Path $xsearchPath -ChildPath 'shared'
$searchOptionsPath = Join-Path -Path $sharedPath -ChildPath 'searchoptions.json'
#endregion


#region SearchSettings
########################################
# SearchSettings
########################################
class SearchSettings : FindSettings {
    [bool]$Colorize
    [bool]$FirstMatch
    [string[]]$InLinesAfterPatterns
    [string[]]$InLinesBeforePatterns
    [Color]$LineColor
    [int]$LinesAfter
    [string[]]$LinesAfterToPatterns
    [string[]]$LinesAfterUntilPatterns
    [int]$LinesBefore
    [int]$MaxLineLength
    [bool]$MultiLineSearch
    [Regex[]]$OutLinesAfterPatterns
    [Regex[]]$OutLinesBeforePatterns
    [bool]$PrintLines
    [bool]$PrintMatches
    [bool]$PrintResults
    [bool]$SearchArchives
    [Regex[]]$SearchPatterns
    [string]$TextFileEncoding
    [bool]$UniqueLines

    SearchSettings() {
		$this.ArchivesOnly = $false
		$this.Colorize = $true
		$this.Debug = $false
		$this.DirColor = [Color]::Cyan
		$this.ExtColor = [Color]::Yellow
		$this.FileColor = [Color]::Magenta
		$this.FirstMatch = $false
		$this.FollowSymlinks = $false
		$this.InArchiveExtensions = @()
		$this.InArchiveFilePatterns = @()
		$this.InDirPatterns = @()
		$this.InExtensions = @()
		$this.InFilePatterns = @()
		$this.InFileTypes = @()
		$this.InLinesAfterPatterns = @()
		$this.InLinesBeforePatterns = @()
		$this.IncludeArchives = $false
		$this.IncludeHidden = $false
        $this.LineColor = [Color]::Green
		$this.LinesAfter = 0
		$this.LinesAfterToPatterns = @()
		$this.LinesAfterUntilPatterns = @()
		$this.LinesBefore = 0
		$this.MaxDepth = -1
		$this.MaxSize = 0
		$this.MaxLineLength = 0
		$this.MinDepth = -1
		$this.MinSize = 0
		$this.MultiLineSearch = $false
		$this.OutArchiveExtensions = @()
		$this.OutArchiveFilePatterns = @()
		$this.OutDirPatterns = @()
		$this.OutExtensions = @()
		$this.OutFilePatterns = @()
		$this.OutFileTypes = @()
		$this.OutLinesAfterPatterns = @()
		$this.OutLinesBeforePatterns = @()
		$this.Paths = @()
		$this.PrintDirs = $false
		$this.PrintFiles = $false
		$this.PrintLines = $false
		$this.PrintMatches = $false
		$this.PrintResults = $false
		$this.PrintUsage = $false
		$this.PrintVersion = $false
		$this.Recursive = $true
		$this.SearchArchives = $false
		$this.SearchPatterns = @()
		$this.SortBy = [SortBy]::FilePath
		$this.SortCaseInsensitive = $false
		$this.SortDescending = $false
		$this.TextFileEncoding = "utf-8"
		$this.UniqueLines = $false
		$this.Verbose = $false
    }

    [string]ToString() {
        return "SearchSettings(" +
        "ArchivesOnly=$($this.ArchivesOnly)" +
        ", Colorize=$($this.Colorize)" +
        ", Debug=$($this.Debug)" +
        ", FirstMatch=$($this.FirstMatch)" +
        ", FollowSymlinks=$($this.FollowSymlinks)" +
        ", InArchiveExtensions=$($this.StringArrayToString($this.InArchiveExtensions))" +
        ", InArchiveFilePatterns=$($this.StringArrayToString($this.InArchiveFilePatterns))" +
        ", InDirPatterns=$($this.StringArrayToString($this.InDirPatterns))" +
        ", InExtensions=$($this.StringArrayToString($this.InExtensions))" +
        ", InFilePatterns=$($this.StringArrayToString($this.InFilePatterns))" +
        ", InFileTypes=$($this.FileTypeArrayToString($this.InFileTypes))" +
        ", InLinesAfterPatterns=$($this.StringArrayToString($this.InLinesAfterPatterns))" +
        ", InLinesBeforePatterns=$($this.StringArrayToString($this.InLinesBeforePatterns))" +
        ", IncludeArchives=$($this.IncludeArchives)" +
        ", IncludeHidden=$($this.IncludeHidden)" +
        ", LinesAfter=$($this.LinesAfter)" +
        ", LinesAfterToPatterns=$($this.StringArrayToString($this.LinesAfterToPatterns))" +
        ", LinesAfterUntilPatterns=$($this.StringArrayToString($this.LinesAfterUntilPatterns))" +
        ", LinesBefore=$($this.LinesBefore)" +
        ", MaxDepth=$($this.MaxDepth)" +
        ", MaxLastMod=$($this.DateTimeToString($this.MaxLastMod))" +
        ", MaxLineLength=$($this.MaxLineLength)" +
        ", MaxSize=$($this.MaxSize)" +
        ", MinDepth=$($this.MinDepth)" +
        ", MinLastMod=$($this.DateTimeToString($this.MinLastMod))" +
        ", MinSize=$($this.MinSize)" +
        ", MultiLineSearch=$($this.MultiLineSearch)" +
        ", OutArchiveExtensions=$($this.StringArrayToString($this.OutArchiveExtensions))" +
        ", OutArchiveFilePatterns=$($this.StringArrayToString($this.OutArchiveFilePatterns))" +
        ", OutDirPatterns=$($this.StringArrayToString($this.OutDirPatterns))" +
        ", OutExtensions=$($this.StringArrayToString($this.OutExtensions))" +
        ", OutFilePatterns=$($this.StringArrayToString($this.OutFilePatterns))" +
        ", OutFileTypes=$($this.FileTypeArrayToString($this.OutFileTypes))" +
        ", OutLinesAfterPatterns=$($this.StringArrayToString($this.OutLinesAfterPatterns))" +
        ", OutLinesBeforePatterns=$($this.StringArrayToString($this.OutLinesBeforePatterns))" +
        ", Paths=$($this.StringArrayToString($this.Paths))" +
        ", PrintDirs=$($this.PrintDirs)" +
        ", PrintFiles=$($this.PrintFiles)" +
        ", PrintLines=$($this.PrintLines)" +
        ", PrintMatches=$($this.PrintMatches)" +
        ", PrintResults=$($this.PrintResults)" +
        ", PrintUsage=$($this.PrintUsage)" +
        ", PrintVersion=$($this.PrintVersion)" +
        ", Recursive=$($this.Recursive)" +
        ", SearchArchives=$($this.SearchArchives)" +
        ", SearchPatterns=$($this.StringArrayToString($this.SearchPatterns))" +
        ", SortBy=$(SortByToName($this.SortBy))" +
        ", SortCaseInsensitive=$($this.SortCaseInsensitive)" +
        ", SortDescending=$($this.SortDescending)" +
        ", TextFileEncoding=`"$($this.TextFileEncoding)`"" +
        ", UniqueLines=$($this.UniqueLines)" +
        ", Verbose=$($this.Verbose)" +
        ")"
    }
}
#endregion


#region SearchOptions
########################################
# SearchOptions
########################################
class SearchOption : Option {
    [string]$SortArg

    SearchOption([string]$ShortArg, [string]$LongArg, [string]$Desc, [ArgTokenType]$ArgType) {
        $this.ShortArg = $ShortArg
        $this.LongArg = $LongArg
        $this.Desc = $Desc
        $this.ArgType = $ArgType
        $this.SortArg = $this.GetSortArg()
    }

    [string]GetSortArg() {
        if ($this.ShortArg) {
            return "$($this.ShortArg.ToLower())a$($this.LongArg)"
        }
        return $this.LongArg
    }
}

class SearchOptions {
    [SearchOption[]]$SearchOptions = @()
    [ArgTokenizer]$ArgTokenizer
    # instantiate this way to get case sensitivity of keys
    $BoolActionMap = @{
        "allmatches" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.FirstMatch = !$b
        }
        "archivesonly" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SetArchivesOnly($b)
        }
        "colorize" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.Colorize = $b
        }
        "debug" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SetDebug($b)
        }
        "excludearchives" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.IncludeArchives = !$b
        }
        "excludehidden" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.IncludeHidden = !$b
        }
        "firstmatch" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.FirstMatch = $b
        }
        "followsymlinks" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.FollowSymlinks = $b
        }
        "help" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintUsage = $b
        }
        "includearchives" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.IncludeArchives = $b
        }
        "includehidden" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.IncludeHidden = $b
        }
        "multilinesearch" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.MultiLineSearch = $b
        }
        "nocolorize" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.Colorize = !$b
        }
        "nofollowsymlinks" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.FollowSymlinks = !$b
        }
        "noprintdirs" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintDirs = !$b
        }
        "noprintfiles" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintFiles = !$b
        }
        "noprintlines" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintLines = !$b
        }
        "noprintmatches" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintMatches = !$b
        }
        "noprintresults" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintResults = !$b
        }
        "norecursive" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.Recursive = !$b
        }
        "nosearcharchives" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SearchArchives = !$b
        }
        "printdirs" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintDirs = $b
        }
        "printfiles" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintFiles = $b
        }
        "printlines" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintLines = $b
        }
        "printmatches" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintMatches = $b
        }
        "printresults" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintResults = $b
        }
        "recursive" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.Recursive = $b
        }
        "searcharchives" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SearchArchives = $b
        }
        "sort-ascending" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SortDescending = !$b
        }
        "sort-caseinsensitive" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SortCaseInsensitive = $b
        }
        "sort-casesensitive" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SortCaseInsensitive = !$b
        }
        "sort-descending" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.SortDescending = $b
        }
        "uniquelines" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.UniqueLines = $b
        }
        "verbose" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.Verbose = $b
        }
        "version" = {
            param([bool]$b, [SearchSettings]$settings)
            $settings.PrintVersion = $b
        }
    }
    $StringActionMap = @{
        "encoding" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.TextFileEncoding = $s
        }
        "in-archiveext" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InArchiveExtensions += $settings.GetExtensions($s)
        }
        "in-archivefilepattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InArchiveFilePatterns += [regex]$s
        }
        "in-dirpattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InDirPatterns += [regex]$s
        }
        "in-ext" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InExtensions += $settings.GetExtensions($s)
        }
        "in-filepattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InFilePatterns += [regex]$s
        }
        "in-filetype" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InFileTypes += GetFileTypeFromName($s)
        }
        "in-linesafterpattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InLinesAfterPatterns += [regex]$s
        }
        "in-linesbeforepattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.InLinesBeforePatterns += [regex]$s
        }
        "linesaftertopattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.LinesAfterToPatterns += [regex]$s
        }
        "linesafteruntilpattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.LinesAfterUntilPatterns += [regex]$s
        }
        "maxlastmod" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.MaxLastMod = [DateTime]$s
        }
        "minlastmod" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.MinLastMod = [DateTime]$s
        }
        "out-archiveext" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutArchiveExtensions += $settings.GetExtensions($s)
        }
        "out-archivefilepattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutArchiveFilePatterns += [regex]$s
        }
        "out-dirpattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutDirPatterns += [regex]$s
        }
        "out-ext" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutExtensions += $settings.GetExtensions($s)
        }
        "out-filepattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutFilePatterns += [regex]$s
        }
        "out-filetype" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutFileTypes += GetFileTypeFromName($s)
        }
        "out-linesafterpattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutLinesAfterPatterns += [regex]$s
        }
        "out-linesbeforepattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.OutLinesBeforePatterns += [regex]$s
        }
        "path" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.Paths += $s
        }
        "searchpattern" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.SearchPatterns += [regex]$s
        }
        "settings-file" = {
            param([string]$s, [SearchSettings]$settings)
            $this.UpdateSettingsFromFilePath($settings, $s)
        }
        "sort-by" = {
            param([string]$s, [SearchSettings]$settings)
            $settings.SortBy = GetSortByFromName($s)
        }
    }
    $IntActionMap = @{
        "linesafter" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.LinesAfter = $i
        }
        "linesbefore" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.LinesBefore = $i
        }
        "maxdepth" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.MaxDepth = $i
        }
        "maxlinelength" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.MaxLineLength = $i
        }
        "maxsize" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.MaxSize = $i
        }
        "mindepth" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.MinDepth = $i
        }
        "minsize" = {
            param([int]$i, [SearchSettings]$settings)
            $settings.MinSize = $i
        }
    }

    SearchOptions() {
        $this.SearchOptions = $this.LoadOptionsFromJson()
        $this.ArgTokenizer = [ArgTokenizer]::new($this.SearchOptions)
    }
    
    [SearchOption[]]LoadOptionsFromJson() {
        $optionsHash = Get-Content $script:searchOptionsPath | ConvertFrom-Json -AsHashtable
        if (-not $optionsHash.ContainsKey('searchoptions')) {
            throw "Missing searchoptions in JSON"
        }
        $opts = @(foreach ($optionObj in $optionsHash['searchoptions']) {
            $ShortArg = ''
            $LongArg = $optionObj['long']
            $Desc = $optionObj['desc']
            $ArgType = [ArgTokenType]::Unknown
            if ($this.BoolActionMap.ContainsKey($LongArg)) {
                $ArgType = [ArgTokenType]::Bool
            } elseif ($this.StringActionMap.ContainsKey($LongArg)) {
                $ArgType = [ArgTokenType]::Str
            } elseif ($this.IntActionMap.ContainsKey($LongArg)) {
                $ArgType = [ArgTokenType]::Int
            }
            if ($optionObj.ContainsKey('short')) {
                $ShortArg = $optionObj['short']
            }
            [SearchOption]::new($ShortArg, $LongArg, $Desc, $ArgType)
        })
        return $opts
    }

    [void]UpdateSettingsFromArgTokens([SearchSettings]$settings, [ArgToken[]]$argTokens) {
        foreach ($argToken in $argTokens) {
            if ($argToken.Type -eq [ArgTokenType]::Bool) {
                if ($this.BoolActionMap.ContainsKey($argToken.Name)) {
                    if ($argToken.Value -is [bool]) {
                        $this.BoolActionMap[$argToken.Name].Invoke($argToken.Value, $settings)
                    } else {
                        throw "Invalid value for option: " + $argToken.Name
                    }
                } else {
                    throw "Invalid option: " + $argToken.Name
                }
            } elseif ($argToken.Type -eq [ArgTokenType]::Str) {
                if ($this.StringActionMap.ContainsKey($argToken.Name)) {
                    if ($argToken.Value -is [string]) {
                        $this.StringActionMap[$argToken.Name].Invoke($argToken.Value, $settings)
                    } else {
                        throw "Invalid value for option: " + $argToken.Name
                    }
                } else {
                    throw "Invalid option: " + $argToken.Name
                }
            } elseif ($argToken.Type -eq [ArgTokenType]::Int) {
                if ($this.IntActionMap.ContainsKey($argToken.Name)) {
                    if ($argToken.Value -is [int] -or $argToken.Value -is [int64]) {
                        $this.IntActionMap[$argToken.Name].Invoke($argToken.Value, $settings)
                    } else {
                        throw "Invalid value for option: " + $argToken.Name
                    }
                } else {
                    throw "Invalid option: " + $argToken.Name
                }
            } else {
                throw "Invalid option: " + $argToken.Name
            }
        }
    }

    [void]UpdateSettingsFromJson([SearchSettings]$settings, [string]$json) {
        $argTokens = $this.ArgTokenizer.TokenizeJson($json)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [void]UpdateSettingsFromFilePath([SearchSettings]$settings, [string]$filePath) {
        $argTokens = $this.ArgTokenizer.TokenizeFilePath($filePath)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [void]UpdateSettingsFromArgs([SearchSettings]$settings, [string[]]$argList) {
        $argTokens = $this.ArgTokenizer.TokenizeArgs($argList)
        $this.UpdateSettingsFromArgTokens($settings, $argTokens)
    }

    [SearchSettings]SettingsFromArgs([string[]]$argList) {
        $settings = [SearchSettings]::new()
        # default PrintResults to true since we're using via CLI
        $settings.PrintResults = $true
        $this.UpdateSettingsFromArgs($settings, $argList)
        return $settings
    }

    [string]GetUsageString() {
        $usage = "`nUsage:`n ps1search [options] -s <searchpattern> <path> [<path> ...]`n`nOptions:`n";
        $optStrs = @()
        $optMap = @{}
        $longest = 0
        $options = $this.SearchOptions | Sort-Object -Property SortArg

        foreach ($option in $options) {
            $optStr = ''
            if ($option.ShortArg) {
                $optStr = "-$($option.ShortArg),"
            }
            $optStr += "--$($option.LongArg)"
            if ($optStr.Length -gt $longest) {
                $longest = $optStr.Length
            }
            $optStrs += $optStr
            $optMap[$optStr] = $option.Desc
        }
        $formatStr = " {0,-$($longest)}  {1}`n"
        foreach ($optStr in $optStrs) {
            $usage += $formatStr -f $optStr,$optMap[$optStr]
        }
        return $usage;
    }
}
#endregion


#region SearchResult
########################################
# SearchResult
########################################
class SearchResult {
    [regex]$SearchPattern
    [FileResult]$File
    [int]$LineNum
    [int]$MatchStartIndex
    [int]$MatchEndIndex
    [string]$Line
    [string[]]$LinesBefore
    [string[]]$LinesAfter

    SearchResult([regex]$SearchPattern, [FileResult]$File, [int]$LineNum, [int]$MatchStartIndex,
                 [int]$MatchEndIndex, [string]$Line, [string[]]$LinesBefore, [string[]]$LinesAfter) {
        $this.SearchPattern = $SearchPattern
        $this.LineNum = $LineNum
        $this.File = $File
        $this.LineNum = $LineNum
        $this.MatchStartIndex = $MatchStartIndex
        $this.MatchEndIndex = $MatchEndIndex
        $this.Line = $Line
        $this.LinesBefore = $LinesBefore
        $this.LinesAfter = $LinesAfter
    }

    SearchResult([regex]$SearchPattern, [FileResult]$File, [int]$LineNum, [int]$MatchStartIndex,
                 [int]$MatchEndIndex, [string]$Line) {
        $this.SearchPattern = $SearchPattern
        $this.LineNum = $LineNum
        $this.File = $File
        $this.LineNum = $LineNum
        $this.MatchStartIndex = $MatchStartIndex
        $this.MatchEndIndex = $MatchEndIndex
        $this.Line = $Line
        $this.LinesBefore = @()
        $this.LinesAfter = @()
    }
}
#endregion


#region FormattedResult
########################################
# FormattedResult
########################################
class FormattedResult {
    [string[]]$LinesBefore
    [string[]]$MatchingLine
    [string[]]$LinesAfter

    FormattedResult([string[]]$matchingLine, [string[]]$linesBefore, [string[]]$linesAfter) {
        $this.MatchingLine = $matchingLine
        $this.LinesBefore = $linesBefore
        $this.LinesAfter = $linesAfter
    }

#    FormattedResult([string[]]$matchingLine) {
#        [FormattedResult]::new($matchingLine, @(), @())
#    }
}
#endregion


#region SearchResultFormatter
########################################
# SearchResultFormatter
########################################
class SearchResultFormatter {
    [SearchSettings]$Settings
    [FileResultFormatter]$FileFormatter
    [Scriptblock]$FormatLineBlock
    [Scriptblock]$FormatMatchBlock

    SearchResultFormatter([SearchSettings]$settings) {
        $this.Settings = $settings
        $this.FileFormatter = [FileResultFormatter]::new($settings)
        if ($settings.Colorize) {
            $this.FormatLineBlock = {
                param([string]$line)
                return $this.FormatLineColor($line)
            }
            $this.FormatMatchBlock = {
                param([string]$m)
                return $this.FormatMatchColor($m)
            }
        } else {
            $this.FormatLineBlock = {
                param([string]$line)
                return $line
            }
            $this.FormatMatchBlock = {
                param([string]$m)
                return $m
            }
        }
    }

    # This splits a string into 3 segments: before, in, and after, where the 'in' segment should be colorized
    [string[]]ColorizeString([string]$s, [int]$matchStartIdx, [int]$matchEndIdx, [Color]$color) {
        return $this.FileFormatter.ColorizeString($s, $matchStartIdx, $matchEndIdx, $color)
    }

    [string]FormatLineColor([string]$line) {
        $formattedLine = $line
        foreach ($pattern in $this.Settings.SearchPatterns) {
            $match = $pattern.Match($formattedLine)
            if ($match.Success) {
                $formattedLine = $this.ColorizeString($formattedLine, $match.Index, $match.Index + $match.Length, $this.Settings.LineColor)
                break
            }
        }
        return $formattedLine
    }

    [string]FormatLine([string]$line) {
        return $this.FormatLineBlock.Invoke($line)
    }

    [string]FormatMatchColor([string]$match) {
        return $this.ColorizeString($match, 0, $match.Length, $this.Settings.LineColor)
    }

    [string]FormatMatch([string]$match) {
        return $this.FormatMatchBlock.Invoke($match)
    }

    [string[]]FormatMatchingLine([SearchResult]$result) {
        $s = $result.Line.TrimEnd()
        $withLeadWhitespaceLen = $s.Length
        $s = $s.TrimStart()
        $formattedLen = $s.Length
        $leadingWhitespaceCount = $withLeadWhitespaceLen - $formattedLen
        $maxLineEndIdx = $formattedLen - 1
        $matchLen = $result.MatchEndIndex - $result.MatchStartIndex
        $matchStartIdx = $result.MatchStartIndex - 1 - $leadingWhitespaceCount
        $matchEndIdx = $matchStartIdx + $matchLen
        [string[]]$elems = @()
        if ($this.Settings.Colorize) {
            $lineElems = $this.ColorizeString($s, $matchStartIdx, $matchEndIdx, $this.Settings.LineColor)
            $elems += $lineElems
        } else {
            $elems += $s
        }
        return $elems
    }
    
    [int]LineNumPadding([SearchResult]$result) {
        $maxLineNum = $result.LineNum + $result.LinesAfter.Count
        return "$maxLineNum".Length
    }

    [FormattedResult]MultiLineFormat([SearchResult]$result) {
        [string[]]$matchingLine = @()
        [string[]]$linesBefore = @()
        [string[]]$linesAfter = @()
        
        $linesBefore += "=" * 80
        $linesBefore += "$($this.FileFormatter.FormatFileResult($result.File)): $($result.LineNum): [$($result.MatchStartIndex):$($result.MatchEndIndex)]"
        $linesBefore += "-" * 80
        $lineNumPadding = $this.LineNumPadding($result)
        $formatStr = " {0,$lineNumPadding} | {1}"
        $currentLineNum = $result.LineNum
        if ($result.LinesBefore.Count -gt 0) {
            $currentLineNum -= $result.LinesBefore.Count
            foreach ($lineBefore in $result.LinesBefore) {
                $currentLine = " $($formatStr -f $currentLineNum, $lineBefore)"
                $linesBefore += $currentLine
                $currentLineNum++;
            }
        }
        $matchingLine += "> {0,$lineNumPadding} | " -f $currentLineNum
        if ($this.Settings.Colorize) {
            $lineElems = $this.ColorizeString($result.Line, $result.MatchStartIndex - 1, $result.MatchEndIndex - 1, $this.Settings.LineColor)
            $matchingLine += $lineElems
        } else {
            $matchingLine += $result.Line
        }
        if ($result.LinesAfter.Count -gt 0) {
            $currentLineNum++;
            foreach ($lineAfter in $result.LinesAfter) {
                $currentLine = " $($formatStr -f $currentLineNum, $lineAfter)"
                $linesAfter += $currentLine
                $currentLineNum++;
            }
        }
        
        return [FormattedResult]::new($matchingLine, $linesBefore, $linesAfter)
    }

    [FormattedResult]SingleLineFormat([SearchResult]$result) {
        [string[]]$elems = @()
        $s = $this.FileFormatter.FormatFileResult($result.File)
        if ($result.LineNum -eq 0) {
            $s += " matches at [$($result.MatchStartIndex):$($result.MatchEndIndex)]"
            $elems += $s
        } else {
            $s += ": $($result.LineNum): [$($result.MatchStartIndex):$($result.MatchEndIndex)]: "
            $elems += $s
            $elems += $this.FormatMatchingLine($result)
        }
        
        return [FormattedResult]::new($elems, @(), @())
    }

    [FormattedResult]Format([SearchResult]$result) {
        if ($result.LinesBefore.Count -gt 0 -or $result.LinesAfter.Count -gt 0) {
            return $this.MultiLineFormat($result)
        }
        return $this.SingleLineFormat($result)
    }
}
#endregion


#region SearchResultSorter
########################################
# SearchResultSorter
########################################
class SearchResultSorter
{
    [SearchSettings]$Settings

    SearchResultSorter([SearchSettings]$settings)
    {
        $this.Settings = $settings
    }

    [Hashtable[]]GetOrderBySearchFields() {
        return @(
            @{ Expression = { $_.LineNum } },
            @{ Expression = { $_.MatchStartIndex } },
            @{ Expression = { $_.MatchEndIndex } }
        )
    }

    [Hashtable[]]GetOrderByPath() {
        return @(
            @{ Expression = { $_.File.File.DirectoryName } },
            @{ Expression = { $_.File.File.Name } }
        ) + $this.GetOrderBySearchFields()
    }

    [Hashtable[]]GetOrderByName() {
        return @(
            @{ Expression = { $_.File.File.Name } },
            @{ Expression = { $_.File.File.DirectoryName } }
        ) + $this.GetOrderBySearchFields()
    }

    [Hashtable[]]GetOrderBySize() {
        return @( @{ Expression = { $_.File.File.Length } } ) + $this.GetOrderByPath()
    }

    [Hashtable[]]GetOrderByType() {
        return @( @{ Expression = { $_.File.Type } } ) + $this.GetOrderByPath()
    }

    [Hashtable[]]GetOrderByLastMod() {
        return @( @{ Expression = { $_.File.File.LastWriteTimeUtc } } ) + $this.GetOrderByPath()
    }

    [SearchResult[]]Sort([SearchResult[]]$searchResults) {
        $order = @()
        switch (SortByToName($this.Settings.SortBy)) {
            'filename' {
                $order = $this.GetOrderByName()
            }
            'filepath' {
                $order = $this.GetOrderByPath()
            }
            'filesize' {
                $order = $this.GetOrderBySize()
            }
            'filetype' {
                $order = $this.GetOrderByType()
            }
            'lastmod' {
                $order = $this.GetOrderByLastMod()
            }
            Default {
                $order = $this.GetOrderByPath()
            }
        }

        if ($this.Settings.SortDescending) {
            if ($this.Settings.SortCaseInsensitive) {
                return $searchResults |
                        Sort-Object -Descending -Property $order
            }
            return $searchResults |
                    Sort-Object -CaseSensitive -Descending -Property $order
        }

        if ($this.Settings.SortCaseInsensitive) {
            return $searchResults |
                    Sort-Object -Property $order
        }
        return $searchResults |
                Sort-Object -CaseSensitive -Property $order
    }

}


#region Searcher
########################################
# Searcher
########################################
class Searcher {
    [SearchSettings]$Settings
    [Finder]$Finder
    [System.Text.Encoding]$TextFileEncoding
    [System.Text.Encoding]$BinaryFileEncoding = [System.Text.Encoding]::GetEncoding("ISO-8859-1");
    [int]$BatchSize = 255

    Searcher([SearchSettings]$settings) {
        $this.Settings = $settings
        $this.Finder = [Finder]::new($settings)
        $this.ValidateSettings()
        # $this.TextFileEncoding = [System.Text.Encoding]::GetEncoding($settings.TextFileEncoding)
        $this.TextFileEncoding = [System.Text.Encoding]::Default
    }

    [void]ValidateSettings() {
        if ($null -eq $this.Settings.SearchPatterns -or $this.Settings.SearchPatterns.Count -eq 0) {
            throw "No search patterns defined"
        }
        try {
            $this.TextFileEncoding = [System.Text.Encoding]::GetEncoding($this.Settings.TextFileEncoding)
        }
        catch {
            throw "Invalid encoding"
        }
        if ($this.Settings.LinesAfter -lt 0) {
            throw "Invalid linesafter"
        }
        if ($this.Settings.LinesBefore -lt 0) {
            throw "Invalid linesbefore"
        }
        if ($this.Settings.MaxLineLength -lt 0) {
            throw "Invalid maxlinelength"
        }
    }

    [bool]MatchesAnyPattern([string]$s, [regex[]]$patterns) {
        return @($patterns | Where-Object { $s -match $_ }).Count -gt 0
    }

    [bool]AnyMatchesAnyPattern([string[]]$strings, [regex[]]$patterns) {
        return @($strings | Where-Object { $this.MatchesAnyPattern($_, $patterns) }).Count -gt 0
    }

    [bool]LinesMatch([string[]]$lines, [regex[]]$inPatterns, [regex[]]$outPatterns) {
        return (($inPatterns.Count -eq 0 -or $this.AnyMatchesAnyPattern($lines, $inPatterns)) -and
                ($outPatterns.Count -eq 0 -or -not $this.AnyMatchesAnyPattern($lines, $outPatterns)))
    }

    [bool]LinesAfterMatch([string[]]$linesAfter) {
        return $this.LinesMatch($linesAfter, $this.Settings.InLinesAfterPatterns, $this.Settings.OutLinesAfterPatterns)
    }

    [bool]LinesBeforeMatch([string[]]$linesBefore) {
        return $this.LinesMatch($linesBefore, $this.Settings.InLinesBeforePatterns, $this.Settings.OutLinesBeforePatterns)
    }

    [SearchResult[]]SearchLines([string[]]$lines) {
        $patternMatches = @{}
        [SearchResult[]]$searchResults = @()
        [int]$lineIdx = 0
        $linesBefore = New-Object System.Collections.Generic.Queue[string]
        $linesAfter = New-Object System.Collections.Generic.Queue[string]
        
        while ($lineIdx -lt $lines.Count -or $linesAfter.Count -gt 0) {
            $line = $linesAfter.Count -gt 0 ? $linesAfter.Dequeue() : $lines[$lineIdx]
            if ($this.Settings.LinesAfter -gt 0) {
                $nextLineIdx = $lineIdx + $linesAfter.Count + 1
                while ($linesAfter.Count -lt $this.Settings.LinesAfter -and $nextLineIdx -lt $lines.Count) {
                    $linesAfter.Enqueue($lines[$nextLineIdx])
                    $nextLineIdx++
                }
            }

            if (($this.Settings.LinesBefore -eq 0 -or $linesBefore.Count -eq 0 -or $this.LinesBeforeMatch($linesBefore)) -and
                ($this.Settings.LinesAfter -eq 0 -or $linesAfter.Count -eq 0 -or $this.LinesAfterMatch($linesAfter))) {

                foreach ($p in $this.Settings.SearchPatterns) {
                    $_matches = @()
                    if ($this.Settings.FirstMatch) {
                        $m = $p.Match($line)
                        if ($m.Success) {
                            $_matches += $m
                            $patternMatches[$p] = 1
                        }
                    } else {
                        $_matches = $p.Matches($line)
                    }
                    foreach ($m in $_matches) {
                        [SearchResult]$searchResult = [SearchResult]::new($p, $null, $lineIdx + 1,
                                $m.Index + 1, $m.Index + $m.Length + 1, $line, $linesBefore.ToArray(),
                                $linesAfter.ToArray())
                        $searchResults += $searchResult
                    }
                }
            }
            
            # If all patterns are in $patternMatches, FirstMatch complete, return $searchResults
            if ($this.Settings.FirstMatch -and $patternMatches.Count -eq $this.Settings.SearchPatterns.Count) {
                return $searchResults
            }

            if ($this.Settings.LinesBefore -gt 0) {
                if ($linesBefore.Count -eq $this.Settings.LinesBefore) {
                    $linesBefore.Dequeue()
                }
                if ($linesBefore.Count -lt $this.Settings.LinesBefore) {
                    $linesBefore.Enqueue($line)
                }
            }

            $lineIdx++
        }

        return $searchResults
    }

    [SearchResult[]]SearchTextFileLines([FileResult]$file) {
        [SearchResult[]]$searchResults = @()
        $lines = Get-Content $file.File.ToString() -Encoding $this.TextFileEncoding
        $searchResults = $this.SearchLines($lines)
        foreach ($searchResult in $searchResults) {
            $searchResult.File = $file
        }

        return $searchResults
    }

    [SearchResult[]]SearchTextFile([FileResult]$file) {
        [SearchResult[]]$searchResults = @()
        if ($this.Settings.MultiLineSearch) {
            $searchResults = $this.SearchTextFileLines($file)
        } else {
            $searchResults = $this.SearchTextFileLines($file)
        }

        return $searchResults
    }

    [SearchResult[]]SearchBinaryFile([FileResult]$file)
    {
        [SearchResult[]]$searchResults = @()
        $content = Get-Content $file.File.ToString() -Encoding $this.BinaryFileEncoding -Raw
        foreach ($p in $this.Settings.SearchPatterns) {
            if ($this.Settings.FirstMatch) {
                $m = $p.Match($content)
                if ($m.Success) {
                    [SearchResult]$searchResult = [SearchResult]::new($p, $file, 0, $m.Index + 1, $m.Index + $m.Length + 1, '')
                    $searchResults += $searchResult
                }
            } else {
                $_matches = $p.Matches($content)
                foreach ($m in $_matches) {
                    [SearchResult]$searchResult = [SearchResult]::new($p, $file, 0, $m.Index + 1, $m.Index + $m.Length + 1, '')
                    $searchResults += $searchResult
                }
            }
        }
        return $searchResults
    }

    [SearchResult[]]SearchFile([FileResult]$file) {
        #Write-Host "SearchFile($($file.ToString()))"
        if ($file.Type -eq [FileType]::Archive) {
            return @()
        }
        elseif ($file.Type -eq [FileType]::Binary)
        {
            return $this.SearchBinaryFile($file)
        }
        elseif ($file.Type -eq [FileType]::Code -or $file.Type -eq [FileType]::Text -or $file.Type -eq [FileType]::Xml)
        {
            return $this.SearchTextFile($file)
        }
        return @()
    }

    [SearchResult[]] SearchBatchParallel([FileResult[]]$fileResultsBatch) {
        # Ensure ThreadJobs module is available
        Import-Module ThreadJob -ErrorAction Stop

        # Shared collection for search results
        $searchResults = [System.Collections.Concurrent.ConcurrentBag[SearchResult]]::new()

        # Process files in parallel
        $fileResultsBatch | ForEach-Object -Parallel {
            param ($file, $instanceSerialized, $resultsBagSerialized)

            # Deserialize the instance and the results bag
            $instance = [System.Management.Automation.PSSerializer]::Deserialize($instanceSerialized)
            $resultsBag = [System.Management.Automation.PSSerializer]::Deserialize($resultsBagSerialized)

            try {
                # Call the SearchFile method
                [SearchResult[]]$results = $instance.SearchFile($file)
                if ($results) {
                    foreach ($result in $results) {
                        $resultsBag.Add($result)
                    }
                }
            } catch {
                Write-Error "Error processing file: $file. Exception: $_"
            }
        } -ThrottleLimit 4 -ArgumentList (
        $fileResultsBatch,
        [System.Management.Automation.PSSerializer]::Serialize($this),
        [System.Management.Automation.PSSerializer]::Serialize($searchResults)
        )

        # Return the combined results as an array
        return $searchResults.ToArray()
    }

    [SearchResult[]] SearchBatchJobs([FileResult[]]$fileResultsBatch) {
        [System.Management.Automation.Job[]]$jobs = @()

        foreach ($file in $fileResultsBatch) {
            [ScriptBlock]$block = {
                param ($instanceSerialized, $file)

                # Deserialize the instance and the results bag
                $instance = [System.Management.Automation.PSSerializer]::Deserialize($instanceSerialized)

                Write-Host "Starting job for file: $file"
                try {
                    [SearchResult[]]$results = $instance.SearchFile($file)
                    Write-Host "Job finished for file: $file"
                    return $results
                } catch {
                    Write-Error "Error in job for file: $file. Exception: $_"
                    return @()
                }
            }
            [System.Management.Automation.Job]$job = Start-Job -ScriptBlock $block -ArgumentList (
                [System.Management.Automation.PSSerializer]::Serialize($this), $file)
#            [ScriptBlock]$block = {
#                param ($searchFunction, $file)
#                Write-Host "Starting job for file: $file"
#                $searchResults = @()
#                try {
#                    $searchResults = & $searchFunction.Invoke($file)
#                } catch {
#                    Write-Error "Error in job for file: $file. Exception: $_"
#                }
#                return $searchResults
#            }
#            $searchFunction = { param ($file) $this.SearchFile($file) }
#            [System.Management.Automation.Job]$job = Start-Job -ScriptBlock $block -ArgumentList $searchFunction, $file
            $jobs += $job
        }

        Write-Host "Waiting for all jobs to complete..."
        $jobs | Wait-Job -Timeout 60

        foreach ($job in $jobs) {
            Write-Host "Job ID: $($job.Id), State: $($job.State)"
            if ($job.State -eq "Failed") {
                Write-Error "Job ID: $($job.Id) failed. Inspect logs for more details."
            }
        }

        [SearchResult[]]$searchResults = @()
        foreach ($job in $jobs) {
            $jobResults = Receive-Job -Job $job
#            [SearchResult[]]$jobResults = Receive-Job -Job $job
            Write-Host "jobResults: $jobResults"
            if ($null -ne $jobResults) {
                $searchResults += $jobResults
            }
        }

        $jobs | Remove-Job
        return $searchResults
    }

    [SearchResult[]] SearchFiles([FileResult[]]$files)
    {
        [SearchResult[]]$searchResults = @()
        foreach ($file in $files)
        {
            $results = $this.SearchFile($file)
            if ($null -ne $results) {
                $searchResults += $results
            }
        }
        return $searchResults
    }

    [SearchResult[]]Search() {
        [FileResult[]]$fileResults = $this.Finder.Find()
        [SearchResult[]]$searchResults = @()

        [int]$searched = 0
        while ($fileResults.Count - $searched -gt $this.BatchSize) {
            $batch = $fileResults[$searched..($searched + $this.BatchSize - 1)]
#            $searchResults += $this.SearchBatchParallel($batch)
            $searchResults += $this.SearchFiles($batch)
            $searched += $this.BatchSize
        }

        if ($fileResults.Count -gt $searched) {
            $batch = $fileResults[$searched..$fileResults.Count]
#            $searchResults += $this.SearchBatchParallel($batch)
#            $searchResults += $this.SearchBatchJobs($batch)
            $searchResults += $this.SearchFiles($batch)
            $searched += $this.BatchSize
        }

        if ($searchResults.Count -gt 1) {
            $searchResultSorter = [SearchResultSorter]::new($this.Settings)
            return $searchResultSorter.Sort($searchResults)
        }
        return $searchResults
    }

    [void]WriteFormattedResult([FormattedResult]$formattedResult)
    {
        if ($formattedResult.LinesBefore.Count -gt 0) {
            # it's a multiline result
            $formattedResult.LinesBefore | ForEach-Object { Write-Host $_ }

            # If formattedResult.MatchingLine.Count == 4 then add color to third element and reset before fourth
            if ($formattedResult.MatchingLine.Count -eq 4) {
                Write-Host $formattedResult.MatchingLine[0] -NoNewline
                Write-Host $formattedResult.MatchingLine[1] -NoNewline
                Write-Host $formattedResult.MatchingLine[2] -ForegroundColor Green -NoNewline
                Write-Host $formattedResult.MatchingLine[3]
            } else {
                Write-Host $formattedResult.MatchingLine -Separator ''
            }

            if ($formattedResult.LinesAfter.Count -gt 0) {
                $formattedResult.LinesAfter | ForEach-Object { Write-Host $_ }
            }
            # blank line after
            Write-Host ""

        } else {
            # If formattedResult.MatchingLine.Count == 4 then add color to third element and reset before fourth
            if ($formattedResult.MatchingLine.Count -eq 4) {
                Write-Host $formattedResult.MatchingLine[0] -NoNewline
                Write-Host $formattedResult.MatchingLine[1] -NoNewline
                Write-Host $formattedResult.MatchingLine[2] -ForegroundColor Green -NoNewline
                Write-Host $formattedResult.MatchingLine[3]
            } else {
                Write-Host $formattedResult.MatchingLine -Separator ''
            }
        }
    }

    [void]PrintResults([SearchResult[]]$searchResults, [SearchResultFormatter]$formatter) {
        if ($searchResults.Count -gt 0) {
            Write-Host "`nSearch results ($($searchResults.Count)):"
            # [SearchResultFormatter]$formatter = [SearchResultFormatter]::new($this.Settings)
            foreach ($r in $searchResults) {
                [FormattedResult]$formatted = $formatter.Format($r)
                $this.WriteFormattedResult($formatted)
            }
        } else {
            LogMsg("`nSearch results: 0")
        }
    }

    [FileResult[]]GetMatchingFiles([SearchResult[]]$searchResults) {
        [FileResult[]]$files = @()
        foreach ($r in $searchResults) {
            if ($null -ne $r.File.File.DirectoryName) {
                $dir = $r.File.File.DirectoryName
            }
            if ($files -notcontains $r.File) {
                $files += $r.File
            }
        }
        return $files
    }

    [void]PrintMatchingDirs([SearchResult[]]$searchResults, [SearchResultFormatter]$formatter) {
        [FileResult[]]$files = $this.GetMatchingFiles($searchResults)
        $this.Finder.PrintMatchingDirs($files, $formatter.FileFormatter)
    }

    [void]PrintMatchingFiles([SearchResult[]]$searchResults, [SearchResultFormatter]$formatter) {
        [FileResult[]]$files = $this.GetMatchingFiles($searchResults)
        $this.Finder.PrintMatchingFiles($files, $formatter.FileFormatter)
    }

    [string[]]GetMatchingLines([SearchResult[]]$searchResults, [SearchSettings]$settings) {
        [string[]]$lines = @()
        foreach ($r in $searchResults) {
            $lines += $r.Line.Trim()
        }
        if ($settings.UniqueLines) {
            $lineMap = @{}
            foreach ($line in $lines) {
                $lineMap[$line] = 1
            }
            $lines = $lineMap.Keys
        }
        if ($settings.SortCaseInsensitive) {
            return $lines | Sort-Object
        }
        return $lines | Sort-Object -CaseSensitive
    }

    [void]PrintMatchingLines([SearchResult[]]$searchResults, [SearchResultFormatter]$formatter) {
        [string[]]$lines = $this.GetMatchingLines($searchResults, $formatter.Settings)
        $hdr = $formatter.Settings.UniqueLines ? "Unique matching lines" : "Matching lines"
        if ($lines.Count -gt 0) {
            LogMsg("`n$hdr ($($lines.Count)):")
            foreach ($line in $lines) {
                LogMsg($formatter.FormatLine($line))
            }
        } else {
            LogMsg("`n$($hdr): 0")
        }
    }

    [string[]]GetMatches([SearchResult[]]$searchResults, [SearchSettings]$settings) {
        [string[]]$matches = @()
        foreach ($r in $searchResults) {
            $matches += $r.Line.Substring($r.MatchStartIndex - 1, $r.MatchEndIndex - $r.MatchStartIndex)
        }
        if ($settings.UniqueLines) {
            $matchMap = @{}
            foreach ($m in $matches) {
                $matchMap[$m] = 1
            }
            $matches = $matchMap.Keys
        }
        if ($settings.SortCaseInsensitive) {
            return $matches | Sort-Object
        }
        return $matches | Sort-Object -CaseSensitive
    }

    [void]PrintMatches([SearchResult[]]$searchResults, [SearchResultFormatter]$formatter) {
        [string[]]$matches = $this.GetMatches($searchResults, $formatter.Settings)
        $hdr = $formatter.Settings.UniqueLines ? "Unique matches" : "Matches"
        if ($matches.Count -gt 0) {
            LogMsg("`n$hdr ($($matches.Count)):")
            foreach ($m in $matches) {
                LogMsg($formatter.FormatMatch($m))
            }
        } else {
            LogMsg("`n$($hdr): 0")
        }
    }
}
#endregion
