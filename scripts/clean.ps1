#!/usr/bin/env pwsh
################################################################################
#
# clean.ps1
#
# Clean xsearch language versions
#
################################################################################
param([switch]$help = $false,
      [switch]$lock = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$xsearchScriptPath = $MyInvocation.MyCommand.Path
$xsearchScriptDir = Split-Path $xsearchScriptPath -Parent

. (Join-Path -Path $xsearchScriptDir -ChildPath 'config.ps1')
# . (Join-Path $xsearchScriptDir 'common.ps1')

$xfindScriptDir = Join-Path $xfindPath 'scripts'

. (Join-Path $xfindScriptDir 'clean_functions.ps1')

# args holds the remaining arguments
$langs = $args

if ($langs -contains 'all') {
    $all = $true
}

$hostname = [System.Net.Dns]::GetHostName()

Hdr('xsearch clean script')
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
Log("lock: $lock")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all) {
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Common Functions
########################################

function Usage
{
    Write-Host "`nUsage: clean.ps1 [-help] [-lock] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Clean functions
################################################################################

function CleanXsearchVersion
{
    param([string]$langName, [string]$versionName)

    $langName = (Get-Culture).TextInfo.ToTitleCase($langName.ToLower())

    $functionName = "Clean${langName}Version"

    if (Get-Command $functionName -ErrorAction 'SilentlyContinue') {
        & $functionName $xsearchPath $versionName

        if ($global:CLEAN_LASTEXITCODE -eq 0) {
            Log("$versionName clean succeeded")
            $global:successfulCleans += $versionName
        } else {
            PrintError("$versionName clean failed")
            $global:failedCleans += $versionName
        }
    }
}

function CleanBashSearch
{
    Write-Host
    Hdr('CleanBashSearch')

    CleanXsearchVersion 'bash' 'bashsearch'
}

function CleanCSearch
{
    Write-Host
    Hdr('CleanCSearch')

    CleanXsearchVersion 'c' 'csearch'
}

function CleanCljSearch
{
    Write-Host
    Hdr('CleanCljSearch')

    CleanXsearchVersion 'clojure' 'cljsearch'
}

function CleanCppSearch
{
    Write-Host
    Hdr('CleanCppSearch')

    CleanXsearchVersion 'cpp' 'cppsearch'
}

function CleanCsSearch
{
    Write-Host
    Hdr('CleanCsSearch')

    CleanXsearchVersion 'csharp' 'cssearch'
}

function CleanDartSearch
{
    Write-Host
    Hdr('CleanDartSearch')

    CleanXsearchVersion 'dart' 'dartsearch'
}

function CleanExSearch
{
    Write-Host
    Hdr('CleanExSearch')

    CleanXsearchVersion 'elixir' 'exsearch'
}

function CleanFsSearch
{

    Write-Host
    Hdr('CleanFsSearch')

    CleanXsearchVersion 'fsharp' 'fssearch'
}

function CleanGoSearch
{
    Write-Host
    Hdr('CleanGoSearch')

    CleanXsearchVersion 'go' 'gosearch'
}

function CleanGroovySearch
{
    Write-Host
    Hdr('CleanGroovySearch')

    CleanXsearchVersion 'groovy' 'groovysearch'
}

function CleanHsSearch
{
    Write-Host
    Hdr('CleanHsSearch')

    CleanXsearchVersion 'haskell' 'hssearch'
}

function CleanJavaSearch
{
    Write-Host
    Hdr('CleanJavaSearch')

    CleanXsearchVersion 'java' 'javasearch'
}

function CleanJsSearch
{
    Write-Host
    Hdr('CleanJsSearch')

    CleanXsearchVersion 'javascript' 'jssearch'
}

function CleanKtSearch
{
    Write-Host
    Hdr('CleanKtSearch')

    CleanXsearchVersion 'kotlin' 'ktsearch'
}

function CleanObjcSearch
{
    Write-Host
    Hdr('CleanObjcSearch')

    CleanXsearchVersion 'objc' 'objcsearch'
}

function CleanMlSearch
{
    Write-Host
    Hdr('CleanMlSearch')
    Log('not implemented at this time')
}

function CleanPhpSearch
{
    Write-Host
    Hdr('CleanPhpSearch')

    CleanXsearchVersion 'php' 'phpsearch'
}

function CleanPlSearch
{
    Write-Host
    Hdr('CleanPlSearch')

    CleanXsearchVersion 'perl' 'plsearch'
}

function CleanPs1Search
{
    Write-Host
    Hdr('CleanPs1Search')

    CleanXsearchVersion 'powershell' 'ps1search'
}

function CleanPySearch
{
    Write-Host
    Hdr('CleanPySearch')

    CleanXsearchVersion 'python' 'pysearch'
}

function CleanRbSearch
{
    Write-Host
    Hdr('CleanRbSearch')

    CleanXsearchVersion 'ruby' 'rbsearch'
}

function CleanRsSearch
{
    Write-Host
    Hdr('CleanRsSearch')

    CleanXsearchVersion 'rust' 'rssearch'
}

function CleanScalaSearch
{
    Write-Host
    Hdr('CleanScalaSearch')

    CleanXsearchVersion 'scala' 'scalasearch'
}

function CleanSwiftSearch
{
    Write-Host
    Hdr('CleanSwiftSearch')

    CleanXsearchVersion 'swift' 'swiftsearch'
}

function CleanTsSearch
{
    Write-Host
    Hdr('CleanTsSearch')

    CleanXsearchVersion 'typescript' 'tssearch'
}

function CleanLinux
{
    Write-Host
    Hdr('CleanLinux')

    # CleanBashSearch

    # CleanCSearch

    # CleanCljSearch

    # CleanCppSearch

    CleanCsSearch

    CleanDartSearch

    CleanExSearch

    CleanFsSearch

    CleanGoSearch

    # CleanGroovySearch

    # CleanHsSearch

    CleanJavaSearch

    CleanJsSearch

    CleanKtSearch

    # CleanObjcSearch

    # CleanMlSearch

    CleanPlSearch

    CleanPhpSearch

    CleanPySearch

    CleanRbSearch

    CleanRsSearch

    # CleanScalaSearch

    CleanSwiftSearch

    CleanTsSearch

    PrintCleanResults

    exit
}

function CleanAll
{
    Write-Host
    Hdr('CleanAll')

    # CleanBashSearch

    # CleanCSearch

    CleanCljSearch

    CleanCppSearch

    CleanCsSearch

    CleanDartSearch

    CleanExSearch

    CleanFsSearch

    CleanGoSearch

    # CleanGroovySearch

    CleanHsSearch

    CleanJavaSearch

    CleanJsSearch

    CleanKtSearch

    CleanObjcSearch

    # CleanMlSearch

    CleanPlSearch

    CleanPhpSearch

    CleanPs1Search

    CleanPySearch

    CleanRbSearch

    CleanRsSearch

    CleanScalaSearch

    CleanSwiftSearch

    CleanTsSearch

    PrintCleanResults

    exit
}

################################################################################
# Main function
################################################################################

function CleanMain
{
    param($langs=@())

    if ($langs.Count -eq 0) {
        Usage
    }

    if ($langs -contains 'all') {
        CleanAll
        exit
    }

    ForEach ($lang in $langs) {
        switch ($lang) {
            'linux'      { CleanLinux }
            # 'bash'       { CleanBashSearch }
            # 'c'          { CleanCSearch }
            'clj'        { CleanCljSearch }
            'clojure'    { CleanCljSearch }
            'cpp'        { CleanCppSearch }
            'cs'         { CleanCsSearch }
            'csharp'     { CleanCsSearch }
            'dart'       { CleanDartSearch }
            'elixir'     { CleanExSearch }
            'ex'         { CleanExSearch }
            'fs'         { CleanFsSearch }
            'fsharp'     { CleanFsSearch }
            'go'         { CleanGoSearch }
            # 'groovy'     { CleanGroovySearch }
            'haskell'    { CleanHsSearch }
            'hs'         { CleanHsSearch }
            'java'       { CleanJavaSearch }
            'javascript' { CleanJsSearch }
            'js'         { CleanJsSearch }
            'kotlin'     { CleanKtSearch }
            'kt'         { CleanKtSearch }
            'objc'       { CleanObjcSearch }
            # 'ocaml'      { CleanMlSearch }
            # 'ml'         { CleanMlSearch }
            'perl'       { CleanPlSearch }
            'pl'         { CleanPlSearch }
            'php'        { CleanPhpSearch }
            'powershell' { CleanPs1Search }
            'ps1'        { CleanPs1Search }
            'pwsh'       { CleanPs1Search }
            'py'         { CleanPySearch }
            'python'     { CleanPySearch }
            'rb'         { CleanRbSearch }
            'ruby'       { CleanRbSearch }
            'rs'         { CleanRsSearch }
            'rust'       { CleanRsSearch }
            'scala'      { CleanScalaSearch }
            'swift'      { CleanSwiftSearch }
            'ts'         { CleanTsSearch }
            'typescript' { CleanTsSearch }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }

    PrintCleanResults
}

if ($help) {
    Usage
}

$oldPwd = Get-Location

try {
    if ($all) {
        CleanAll
    }
    
    CleanMain $langs    
} catch {
    PrintError($_.Exception.Message)
} finally {
    Set-Location $oldPwd
}
