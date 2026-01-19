#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language versions of xsearch, or all versions
#
################################################################################
param([switch]$help = $false,
      [switch]$debug = $false,
      [switch]$release = $false,
      [switch]$venv = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$xsearchScriptPath = $MyInvocation.MyCommand.Path
$xsearchScriptDir = Split-Path $xsearchScriptPath -Parent

. (Join-Path -Path $xsearchScriptDir -ChildPath 'config.ps1')
# . (Join-Path $xsearchScriptDir 'common.ps1')

$xfindScriptDir = Join-Path $xfindPath 'scripts'

. (Join-Path -Path $xfindScriptDir -ChildPath 'build_functions.ps1')

if (-not $release) {
    $debug = $true
}

# args holds the remaining arguments
$langs = $args

if ($langs -contains 'all') {
    $all = $true
}

$hostname = [System.Net.Dns]::GetHostName()

Hdr('xsearch build script')
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
Log("debug: $debug")
Log("release: $release")
Log("venv: $venv")
Log("all: $all")
Log("args: $args")
if ($langs.Length -gt 0 -and -not $all) {
    Log("langs ($($langs.Length)): $langs")
}


########################################
# Common Functions
########################################

function Usage
{
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | lang [lang...]}`n"
    exit
}


################################################################################
# Build functions
################################################################################

function BuildXsearchVersion
{
    param([string]$langName, [string]$versionName)

    $langName = (Get-Culture).TextInfo.ToTitleCase($langName.ToLower())

    $functionName = "Build${langName}Version"

    if (Get-Command $functionName -ErrorAction 'SilentlyContinue') {
        & $functionName $xsearchPath $versionName

        if ($global:BUILD_LASTEXITCODE -eq 0) {
            Log("$versionName build succeeded")
            $global:successfulBuilds += $versionName
        } else {
            PrintError("$versionName build failed")
            $global:failedBuilds += $versionName
        }
    }
}

function BuildBashSearch
{
    Write-Host
    Hdr('BuildBashSearch')

    BuildXsearchVersion 'bash' 'bashsearch'
}

function BuildCSearch
{
    Write-Host
    Hdr('BuildCSearch')

    BuildXsearchVersion 'c' 'csearch'
}

function BuildCljSearch
{
    Write-Host
    Hdr('BuildCljSearch')

    BuildXsearchVersion 'clojure' 'cljsearch'
}

function BuildCppSearch
{
    Write-Host
    Hdr('BuildCppSearch')

    BuildXsearchVersion 'cpp' 'cppsearch'
}

function BuildCsSearch
{
    Write-Host
    Hdr('BuildCsSearch')

    BuildXsearchVersion 'csharp' 'cssearch'
}

function BuildDartSearch
{
    Write-Host
    Hdr('BuildDartSearch')

    BuildXsearchVersion 'dart' 'dartsearch'
}

function BuildExSearch
{
    Write-Host
    Hdr('BuildExSearch')

    BuildXsearchVersion 'elixir' 'exsearch'
}

function BuildFsSearch
{
    Write-Host
    Hdr('BuildFsSearch')

    BuildXsearchVersion 'fsharp' 'fssearch'
}

function BuildGoSearch
{
    Write-Host
    Hdr('BuildGoSearch')

    BuildXsearchVersion 'go' 'gosearch'
}

function BuildGroovySearch
{
    Write-Host
    Hdr('BuildGroovySearch')

    BuildXsearchVersion 'groovy' 'groovysearch'
}

function BuildHsSearch
{
    Write-Host
    Hdr('BuildHsSearch')

    BuildXsearchVersion 'haskell' 'hssearch'
}

function BuildJavaSearch
{
    Write-Host
    Hdr('BuildJavaSearch')

    BuildXsearchVersion 'java' 'javasearch'
}

function BuildJsSearch
{
    Write-Host
    Hdr('BuildJsSearch')

    BuildXsearchVersion 'javascript' 'jssearch'
}

function BuildKtSearch
{
    Write-Host
    Hdr('BuildKtSearch')

    BuildXsearchVersion 'kotlin' 'ktsearch'
}

function BuildMlSearch
{
    Write-Host
    Hdr('BuildMlSearch')
    Log("language: ocaml")

    Log("Not currently implemented")
}

function BuildObjcSearch
{
    Write-Host
    Hdr('BuildObjcSearch')

    BuildXsearchVersion 'objc' 'objcsearch'
}

function BuildPlSearch
{
    Write-Host
    Hdr('BuildPlSearch')

    BuildXsearchVersion 'perl' 'plsearch'
}

function BuildPhpSearch
{
    Write-Host
    Hdr('BuildPhpSearch')

    BuildXsearchVersion 'php' 'phpsearch'
}

function BuildPs1Search
{
    Write-Host
    Hdr('BuildPs1Search')

    BuildXsearchVersion 'powershell' 'ps1search'
}

function BuildPySearch
{
    Write-Host
    Hdr('BuildPySearch')

    BuildXsearchVersion 'python' 'pysearch'
}

function BuildRbSearch
{
    Write-Host
    Hdr('BuildRbSearch')

    BuildXsearchVersion 'ruby' 'rbsearch'
}

function BuildRsSearch
{
    Write-Host
    Hdr('BuildRsSearch')

    BuildXsearchVersion 'rust' 'rssearch'
}

function BuildScalaSearch
{
    Write-Host
    Hdr('BuildScalaSearch')

    BuildXsearchVersion 'scala' 'scalasearch'
}

function BuildSwiftSearch
{
    Write-Host
    Hdr('BuildSwiftSearch')

    BuildXsearchVersion 'swift' 'swiftsearch'
}

function BuildTsSearch
{
    Write-Host
    Hdr('BuildTsSearch')

    BuildXsearchVersion 'typescript' 'tssearch'
}

function BuildLinux
{
    Write-Host
    Hdr('BuildLinux')

    # Measure-Command { BuildBashSearch }

    # Measure-Command { BuildCSearch }

    # Measure-Command { BuildCljSearch }

    # Measure-Command { BuildCppSearch }

    Measure-Command { BuildCsSearch }

    Measure-Command { BuildDartSearch }

    Measure-Command { BuildFsSearch }

    Measure-Command { BuildGoSearch }

    Measure-Command { BuildJavaSearch }

    Measure-Command { BuildJsSearch }

    # Measure-Command { BuildKtSearch }

    Measure-Command { BuildPlSearch }

    Measure-Command { BuildPhpSearch }

    # Measure-Command { BuildPs1Search }

    Measure-Command { BuildPySearch }

    Measure-Command { BuildRbSearch }

    Measure-Command { BuildRsSearch }

    # Measure-Command { BuildScalaSearch }

    Measure-Command { BuildSwiftSearch }

    Measure-Command { BuildTsSearch }

    PrintBuildResults

    exit
}

function BuildAll
{
    Write-Host
    Hdr('BuildAll')

    # Measure-Command { BuildBashSearch }

    # Measure-Command { BuildCSearch }

    Measure-Command { BuildCljSearch }

    Measure-Command { BuildCppSearch }

    Measure-Command { BuildCsSearch }

    Measure-Command { BuildDartSearch }

    Measure-Command { BuildExSearch }

    Measure-Command { BuildFsSearch }

    Measure-Command { BuildGoSearch }

    # Measure-Command { BuildGroovySearch }

    Measure-Command { BuildHsSearch }

    Measure-Command { BuildJavaSearch }

    Measure-Command { BuildJsSearch }

    Measure-Command { BuildKtSearch }

    Measure-Command { BuildObjcSearch }

    # Measure-Command { BuildMlSearch }

    Measure-Command { BuildPlSearch }

    Measure-Command { BuildPhpSearch }

    Measure-Command { BuildPs1Search }

    Measure-Command { BuildPySearch }

    Measure-Command { BuildRbSearch }

    Measure-Command { BuildRsSearch }

    Measure-Command { BuildScalaSearch }

    Measure-Command { BuildSwiftSearch }

    Measure-Command { BuildTsSearch }

    PrintBuildResults

    exit
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($langs=@())

    if ($langs.Count -eq 0) {
        Usage
    }

    if ($langs -contains 'all') {
        BuildAll
        exit
    }
    if ($langs -contains 'linux') {
        BuildLinux
        exit
    }

    ForEach ($lang in $langs) {
        switch ($lang.ToLower()) {
            # 'bash'       { Measure-Command { BuildBashSearch } }
            # 'c'          { Measure-Command { BuildCSearch } }
            'clj'        { Measure-Command { BuildCljSearch } }
            'clojure'    { Measure-Command { BuildCljSearch } }
            'cpp'        { Measure-Command { BuildCppSearch } }
            'cs'         { Measure-Command { BuildCsSearch } }
            'csharp'     { Measure-Command { BuildCsSearch } }
            'dart'       { Measure-Command { BuildDartSearch } }
            'elixir'     { Measure-Command { BuildExSearch } }
            'ex'         { Measure-Command { BuildExSearch } }
            'fs'         { Measure-Command { BuildFsSearch } }
            'fsharp'     { Measure-Command { BuildFsSearch } }
            'go'         { Measure-Command { BuildGoSearch } }
            # 'groovy'     { Measure-Command { BuildGroovySearch } }
            'haskell'    { Measure-Command { BuildHsSearch } }
            'hs'         { Measure-Command { BuildHsSearch } }
            'java'       { Measure-Command { BuildJavaSearch } }
            'javascript' { Measure-Command { BuildJsSearch } }
            'js'         { Measure-Command { BuildJsSearch } }
            'kotlin'     { Measure-Command { BuildKtSearch } }
            'kt'         { Measure-Command { BuildKtSearch } }
            'objc'       { Measure-Command { BuildObjcSearch } }
            # 'ocaml'      { Measure-Command { BuildMlSearch } }
            # 'ml'         { Measure-Command { BuildMlSearch } }
            'perl'       { Measure-Command { BuildPlSearch } }
            'pl'         { Measure-Command { BuildPlSearch } }
            'php'        { Measure-Command { BuildPhpSearch } }
            'powershell' { Measure-Command { BuildPs1Search } }
            'ps1'        { Measure-Command { BuildPs1Search } }
            'pwsh'       { Measure-Command { BuildPs1Search } }
            'py'         { Measure-Command { BuildPySearch } }
            'python'     { Measure-Command { BuildPySearch } }
            'rb'         { Measure-Command { BuildRbSearch } }
            'ruby'       { Measure-Command { BuildRbSearch } }
            'rs'         { Measure-Command { BuildRsSearch } }
            'rust'       { Measure-Command { BuildRsSearch } }
            'scala'      { Measure-Command { BuildScalaSearch } }
            'swift'      { Measure-Command { BuildSwiftSearch } }
            'ts'         { Measure-Command { BuildTsSearch } }
            'typescript' { Measure-Command { BuildTsSearch } }
            default      { ExitWithError("unknown/unsupported language: $lang") }
        }
    }

    PrintBuildResults
}

if ($help) {
    Usage
}

$oldPwd = Get-Location

try {
    if ($all) {
        BuildAll
    }

    BuildMain $langs
} catch {
    PrintError($_.Exception.Message)
} finally {
    Set-Location $oldPwd
}
