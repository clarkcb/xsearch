#!/usr/bin/env pwsh
################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xsearch, or all versions
#
################################################################################
param([switch]$help = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$xsearchScriptPath = $MyInvocation.MyCommand.Path
$xsearchScriptDir = Split-Path $xsearchScriptPath -Parent

. (Join-Path -Path $xsearchScriptDir -ChildPath 'config.ps1')
# . (Join-Path $xsearchScriptDir 'common.ps1')

$xfindScriptDir = Join-Path $xfindPath 'scripts'
. (Join-Path $xfindScriptDir 'unittest_functions.ps1')

# args holds the remaining arguments
$langs = $args

if ($langs -contains 'all') {
    $all = $true
}

$hostname = [System.Net.Dns]::GetHostName()

Hdr('xsearch unittest script')
Log("user: $env:USER")
Log("host: $hostname")
if ($IsWindows) {
    Log("os: $env:OS")
} elseif ($IsLinux) {
    Log("os: Linux")
} elseif ($IsMacOS) {
    Log("os: Darwin")
} else {
    Log("os: unknown")
}

$gitBranch = git branch --show-current
$gitCommit = git rev-parse --short HEAD
Log("git branch: $gitBranch ($gitCommit)")

Log("help: $help")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all) {
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Common Functions
########################################

function Usage
{
    Write-Host "`nUsage: unittest.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Unit Test functions
################################################################################

function UnitTestXsearchVersion
{
    param([string]$langName, [string]$versionName)

    $langName = (Get-Culture).TextInfo.ToTitleCase($langName.ToLower())

    $functionName = "UnitTest${langName}Version"

    if (Get-Command $functionName -ErrorAction 'SilentlyContinue') {
        & $functionName $xsearchPath $versionName

        if ($global:UNITTEST_LASTEXITCODE -eq 0) {
            Log("$versionName tests succeeded")
            $global:successfulTests += $versionName
        } else {
            PrintError("$versionName tests failed")
            $global:failedTests += $versionName
        }
    }
}

function UnitTestBashSearch
{
    Write-Host
    Hdr('UnitTestBashSearch')

    UnitTestXsearchVersion 'bash' 'bashsearch'
}

function UnitTestCSearch
{
    Write-Host
    Hdr('UnitTestCSearch')

    UnitTestXsearchVersion 'c' 'csearch'
}

function UnitTestCljSearch
{
    Write-Host
    Hdr('UnitTestCljSearch')

    UnitTestXsearchVersion 'clojure' 'cljsearch'
}

function UnitTestCppSearch
{
    Write-Host
    Hdr('UnitTestCppSearch')

    UnitTestXsearchVersion 'cpp' 'cppsearch'
}

function UnitTestCsSearch
{
    Write-Host
    Hdr('UnitTestCsSearch')

    UnitTestXsearchVersion 'csharp' 'cssearch'
}

function UnitTestDartSearch
{
    Write-Host
    Hdr('UnitTestDartSearch')

    UnitTestXsearchVersion 'dart' 'dartsearch'
}

function UnitTestExSearch
{
    Write-Host
    Hdr('UnitTestExSearch')

    UnitTestXsearchVersion 'elixir' 'exsearch'
}

function UnitTestFsSearch
{
    Write-Host
    Hdr('UnitTestFsSearch')

    UnitTestXsearchVersion 'fsharp' 'fssearch'
}

function UnitTestGoSearch
{
    Write-Host
    Hdr('UnitTestGoSearch')

    UnitTestXsearchVersion 'go' 'gosearch'
}

function UnitTestGroovySearch
{
    Write-Host
    Hdr('UnitTestGroovySearch')

    UnitTestXsearchVersion 'groovy' 'groovysearch'
}

function UnitTestHsSearch
{
    Write-Host
    Hdr('UnitTestHsSearch')

    UnitTestXsearchVersion 'haskell' 'hssearch'
}

function UnitTestJavaSearch
{
    Write-Host
    Hdr('UnitTestJavaSearch')

    UnitTestXsearchVersion 'java' 'javasearch'
}

function UnitTestJsSearch
{
    Write-Host
    Hdr('UnitTestJsSearch')

    UnitTestXsearchVersion 'javascript' 'jssearch'
}

function UnitTestKtSearch
{
    Write-Host
    Hdr('UnitTestKtSearch')

    UnitTestXsearchVersion 'kotlin' 'ktsearch'
}

function UnitTestObjcSearch
{
    Write-Host
    Hdr('UnitTestObjcSearch')

    UnitTestXsearchVersion 'objc' 'objcsearch'
}

function UnitTestMlSearch
{
    Write-Host
    Hdr('UnitTestMlSearch')
    Log('not implemented at this time')
}

function UnitTestPlSearch
{
    Write-Host
    Hdr('UnitTestPlSearch')

    UnitTestXsearchVersion 'perl' 'plsearch'
}

function UnitTestPhpSearch
{
    Write-Host
    Hdr('UnitTestPhpSearch')

    UnitTestXsearchVersion 'php' 'phpsearch'
}

function UnitTestPs1Search
{
    Write-Host
    Hdr('UnitTestPs1Search')

    UnitTestXsearchVersion 'powershell' 'ps1search'
}

function UnitTestPySearch
{
    Write-Host
    Hdr('UnitTestPySearch')

    UnitTestXsearchVersion 'python' 'pysearch'
}

function UnitTestRbSearch
{
    Write-Host
    Hdr('UnitTestRbSearch')

    UnitTestXsearchVersion 'ruby' 'rbsearch'
}

function UnitTestRsSearch
{
    Write-Host
    Hdr('UnitTestRsSearch')

    UnitTestXsearchVersion 'rust' 'rssearch'
}

function UnitTestScalaSearch
{
    Write-Host
    Hdr('UnitTestScalaSearch')

    UnitTestXsearchVersion 'scala' 'scalasearch'
}

function UnitTestSwiftSearch
{
    Write-Host
    Hdr('UnitTestSwiftSearch')

    UnitTestXsearchVersion 'swift' 'swiftsearch'
}

function UnitTestTsSearch
{
    Write-Host
    Hdr('UnitTestTsSearch')

    UnitTestXsearchVersion 'typescript' 'tssearch'
}

function UnitTestAll
{
    Write-Host
    Hdr('UnitTestAll')

    # Measure-Command { UnitTestBashSearch }

    # Measure-Command { UnitTestCSearch }

    Measure-Command { UnitTestCljSearch }

    Measure-Command { UnitTestCppSearch }

    Measure-Command { UnitTestCsSearch }

    Measure-Command { UnitTestDartSearch }

    Measure-Command { UnitTestExSearch }

    Measure-Command { UnitTestFsSearch }

    Measure-Command { UnitTestGoSearch }

    # Measure-Command { UnitTestGroovySearch }

    Measure-Command { UnitTestHsSearch }

    Measure-Command { UnitTestJavaSearch }

    Measure-Command { UnitTestJsSearch }

    Measure-Command { UnitTestKtSearch }

    Measure-Command { UnitTestObjcSearch }

    # Measure-Command { UnitTestMlSearch }

    Measure-Command { UnitTestPlSearch }

    Measure-Command { UnitTestPhpSearch }

    Measure-Command { UnitTestPs1Search }

    Measure-Command { UnitTestPySearch }

    Measure-Command { UnitTestRbSearch }

    Measure-Command { UnitTestRsSearch }

    Measure-Command { UnitTestScalaSearch }

    Measure-Command { UnitTestSwiftSearch }

    Measure-Command { UnitTestTsSearch }

    PrintTestResults

    exit
}

################################################################################
# Main function
################################################################################

function UnitTestMain
{
    param($langs=@())

    if ($langs.Count -eq 0) {
        Usage
    }

    if ($langs -contains 'all') {
        UnitTestAll
    }

    ForEach ($lang in $langs) {
        switch ($lang) {
            # 'bash'       { UnitTestBashSearch }
            # 'bash'       { Measure-Command { UnitTestBashSearch } }
            # 'c'          { Measure-Command { UnitTestCSearch } }
            'clj'        { Measure-Command { UnitTestCljSearch } }
            'clojure'    { Measure-Command { UnitTestCljSearch } }
            'cpp'        { Measure-Command { UnitTestCppSearch } }
            'cs'         { Measure-Command { UnitTestCsSearch } }
            'csharp'     { Measure-Command { UnitTestCsSearch } }
            'dart'       { Measure-Command { UnitTestDartSearch } }
            'elixir'     { Measure-Command { UnitTestExSearch } }
            'ex'         { Measure-Command { UnitTestExSearch } }
            'fs'         { Measure-Command { UnitTestFsSearch } }
            'fsharp'     { Measure-Command { UnitTestFsSearch } }
            'go'         { Measure-Command { UnitTestGoSearch } }
            # 'groovy'     { Measure-Command { UnitTestGroovySearch } }
            'haskell'    { Measure-Command { UnitTestHsSearch } }
            'hs'         { Measure-Command { UnitTestHsSearch } }
            'java'       { Measure-Command { UnitTestJavaSearch } }
            'javascript' { Measure-Command { UnitTestJsSearch } }
            'js'         { Measure-Command { UnitTestJsSearch } }
            'kotlin'     { Measure-Command { UnitTestKtSearch } }
            'kt'         { Measure-Command { UnitTestKtSearch } }
            'objc'       { Measure-Command { UnitTestObjcSearch } }
            # 'ocaml'      { Measure-Command { UnitTestMlSearch } }
            # 'ml'         { Measure-Command { UnitTestMlSearch } }
            'perl'       { Measure-Command { UnitTestPlSearch } }
            'pl'         { Measure-Command { UnitTestPlSearch } }
            'php'        { Measure-Command { UnitTestPhpSearch } }
            'powershell' { Measure-Command { UnitTestPs1Search } }
            'ps1'        { Measure-Command { UnitTestPs1Search } }
            'pwsh'       { Measure-Command { UnitTestPs1Search } }
            'py'         { Measure-Command { UnitTestPySearch } }
            'python'     { Measure-Command { UnitTestPySearch } }
            'rb'         { Measure-Command { UnitTestRbSearch } }
            'ruby'       { Measure-Command { UnitTestRbSearch } }
            'rs'         { Measure-Command { UnitTestRsSearch } }
            'rust'       { Measure-Command { UnitTestRsSearch } }
            'scala'      { Measure-Command { UnitTestScalaSearch } }
            'swift'      { Measure-Command { UnitTestSwiftSearch } }
            'ts'         { Measure-Command { UnitTestTsSearch } }
            'typescript' { Measure-Command { UnitTestTsSearch } }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }

    PrintTestResults
}

if ($help) {
    Usage
}

$oldPwd = Get-Location

try {
    if ($all) {
        UnitTestAll
    }

    UnitTestMain $langs
} catch {
    PrintError($_.Exception.Message)
} finally {
    Set-Location $oldPwd
}
