#!/usr/bin/env pwsh
################################################################################
#
# clean.ps1
#
# Runs a clean (remove generated files) for each language version
#
################################################################################
param([switch]$help = $false,
      [switch]$all = $false)

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path $scriptDir 'config.ps1')
. (Join-Path $scriptDir 'common.ps1')

# args holds the remaining arguments
$langs = $args

if ($langs -contains 'all')
{
    $all = $true
}

Log("help: $help")
Log("all: $all")
if ($langs.Length -gt 0 -and -not $all)
{
    Log("langs ($($langs.Length)): $langs")
}

# Add failed builds to this array and report failed builds at the end
$failedBuilds = @()


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: clean.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}

function CleanJsonResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Extension -eq '.json'}
    ForEach ($f in $resourceFiles)
    {
        Log("Remove-Item $f")
        Remove-Item $f
    }
}

function CleanTestResources
{
    param([string]$resourcesPath)
    $resourceFiles = Get-ChildItem $resourcesPath -Depth 0 | Where-Object {!$_.PsIsContainer -and $_.Name -like "testFile*" -and $_.Extension -eq '.txt'}
    ForEach ($f in $resourceFiles)
    {
        Log("Remove-Item $f")
        Remove-Item $f
    }
}

function PrintFailedBuilds
{
    if ($global:failedBuilds.Length -gt 0)
    {
        $joinedBuilds = $global:failedBuilds -join ', '
        PrintError("Failed builds: $joinedBuilds")
    }
    else
    {
        Log("All builds succeeded")
    }
}


################################################################################
# Clean functions
################################################################################

function CleanBashSearch
{
    Write-Host
    Hdr('CleanBashSearch')
    Log('Nothing to do for bash')
}

function CleanCSearch
{
    Write-Host
    Hdr('CleanCSearch')

    $oldPwd = Get-Location
    Set-Location $cSearchPath

    Log('make clean')
    make clean

    Set-Location $oldPwd
}

function CleanCljSearch
{
    Write-Host
    Hdr('CleanCljSearch')

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        $global:failedBuilds += 'cljsearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $cljSearchPath

    Log('lein clean')
    lein clean

    $resourcesPath = Join-Path $cljSearchPath 'resources'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanCppSearch
{
    Write-Host
    Hdr('CleanCppSearch')

    $oldPwd = Get-Location
    Set-Location $cppSearchPath

    $cmakeBuildDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('cmake-build-')}
    ForEach ($c in $cmakeBuildDirs)
    {
        if (Test-Path $c)
        {
            Log("Remove-Item $c -Recurse -Force")
            Remove-Item $c -Recurse -Force
        }
    }

    Set-Location $oldPwd
}

function CleanCsSearch
{
    Write-Host
    Hdr('CleanCsSearch')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'cssearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $csSearchPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $csSearchProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('CsSearch')}
    ForEach ($p in $csSearchProjectDirs)
    {
        $binDir = Join-Path $p.FullName 'bin'
        if (Test-Path $binDir)
        {
            Log("Remove-Item $binDir -Recurse -Force")
            Remove-Item $binDir -Recurse -Force
        }
        $objDir = Join-Path $p.FullName 'obj'
        if (Test-Path $objDir)
        {
            Log("Remove-Item $objDir -Recurse -Force")
            Remove-Item $objDir -Recurse -Force
        }
    }

    $resourcesPath = Join-Path $csSearchPath 'CsSearchLib' 'Resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $csSearchPath 'CsSearchTests' 'Resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanDartSearch
{
    Write-Host
    Hdr('CleanDartSearch')

    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        $global:failedBuilds += 'dartsearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $dartSearchPath

    Log('dart pub cache repair')
    dart pub cache repair

    Set-Location $oldPwd
}

function CleanExSearch
{
    Write-Host
    Hdr('CleanExSearch')

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        $global:failedBuilds += 'exsearch'
        return
    }

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        $global:failedBuilds += 'exsearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $exSearchPath

    Log('mix clean')
    mix clean

    Set-Location $oldPwd
}

function CleanFsSearch
{

    Write-Host
    Hdr('CleanFsSearch')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'fssearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $fsSearchPath

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    Log("dotnet clean -v minimal")
    dotnet clean -v minimal

    $fsSearchProjectDirs = Get-ChildItem . -Depth 0 | Where-Object {$_.PsIsContainer -and $_.Name.StartsWith('FsSearch')}
    ForEach ($p in $fsSearchProjectDirs)
    {
        $binDir = Join-Path $p.FullName 'bin'
        if (Test-Path $binDir)
        {
            Log("Remove-Item $binDir -Recurse -Force")
            Remove-Item $binDir -Recurse -Force
        }
        $objDir = Join-Path $p.FullName 'obj'
        if (Test-Path $objDir)
        {
            Log("Remove-Item $objDir -Recurse -Force")
            Remove-Item $objDir -Recurse -Force
        }
    }

    $resourcesPath = Join-Path $fsSearchPath 'FsSearchLib' 'Resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $fsSearchPath 'FsSearchTests' 'Resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanGoSearch
{
    Write-Host
    Hdr('CleanGoSearch')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        $global:failedBuilds += 'gosearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $goSearchPath

    Log('go clean')
    go clean

    Set-Location $oldPwd
}

function CleanGroovySearch
{
    Write-Host
    Hdr('CleanGroovySearch')

    $oldPwd = Get-Location
    Set-Location $groovySearchPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'groovysearch'
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $groovySearchPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $groovySearchPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanHsSearch
{
    Write-Host
    Hdr('CleanHsSearch')

    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        $global:failedBuilds += 'hssearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $hsSearchPath

    Log('stack clean')
    stack clean

    $resourcesPath = Join-Path $hsSearchPath 'data'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanJavaSearch
{
    Write-Host
    Hdr('CleanJavaSearch')

    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install maven')
        $global:failedBuilds += 'javasearch'
        return
    }

    Log("mvn -f $javaSearchPath/pom.xml clean")
    mvn -f $javaSearchPath/pom.xml clean

    $resourcesPath = Join-Path $javaSearchPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $javaSearchPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)
}

function CleanJsSearch
{
    Write-Host
    Hdr('CleanJsSearch')

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        $global:failedBuilds += 'jssearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $jsSearchPath

    Log('npm run clean')
    npm run clean

    $resourcesPath = Join-Path $jsSearchPath 'data'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
}

function CleanKtSearch
{
    Write-Host
    Hdr('CleanKtSearch')

    $oldPwd = Get-Location
    Set-Location $ktSearchPath

    $gradle = 'gradle'
    $gradleWrapper = Join-Path '.' 'gradlew'
    if (Test-Path $gradleWrapper)
    {
        $gradle = $gradleWrapper
    }
    elseif (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        $global:failedBuilds += 'ktsearch'
        Set-Location $oldPwd
        return
    }

    Log("$gradle --warning-mode all clean")
    & $gradle --warning-mode all clean

    $resourcesPath = Join-Path $ktSearchPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $ktSearchPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanObjcSearch
{
    Write-Host
    Hdr('CleanObjcSearch')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'objcsearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcSearchPath

    Log("swift package clean")
    swift package clean

    Set-Location $oldPwd
}

function CleanMlSearch
{
    Write-Host
    Hdr('CleanMlSearch')
    Log('not implemented at this time')
}

function CleanPlSearch
{
    Write-Host
    Hdr('CleanPlSearch')

    $resourcesPath = Join-Path $plSearchPath 'share'
    CleanJsonResources($resourcesPath)
}

function CleanPhpSearch
{
    Write-Host
    Hdr('CleanPhpSearch')

    $resourcesPath = Join-Path $phpSearchPath 'resources'
    CleanJsonResources($resourcesPath)
}

function CleanPs1Search
{
    Write-Host
    Hdr('CleanPs1Search')
    Log('Nothing to do for powershell')
}

function CleanPySearch
{
    Write-Host
    Hdr('CleanPySearch')

    $resourcesPath = Join-Path $pySearchPath 'pysearch' 'data'
    CleanJsonResources($resourcesPath)
}

function CleanRbSearch
{
    Write-Host
    Hdr('CleanRbSearch')

    $resourcesPath = Join-Path $rbSearchPath 'data'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $rbSearchPath 'test' 'fixtures'
    CleanTestResources($testResourcesPath)
}

function CleanRsSearch
{
    Write-Host
    Hdr('CleanRsSearch')

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        $global:failedBuilds += 'rssearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $rsSearchPath

    Log('cargo clean')
    cargo clean

    Set-Location $oldPwd
}

function CleanScalaSearch
{
    Write-Host
    Hdr('CleanScalaSearch')

    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala + sbt')
        $global:failedBuilds += 'scalasearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $scalaSearchPath

    Log('sbt clean')
    sbt clean

    $resourcesPath = Join-Path $scalaSearchPath 'src' 'main' 'resources'
    CleanJsonResources($resourcesPath)

    $testResourcesPath = Join-Path $scalaSearchPath 'src' 'test' 'resources'
    CleanTestResources($testResourcesPath)

    Set-Location $oldPwd
}

function CleanSwiftSearch
{
    Write-Host
    Hdr('CleanSwiftSearch')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'swiftsearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $swiftSearchPath

    Log("swift package clean")
    swift package clean

    Set-Location $oldPwd
}

function CleanTsSearch
{
    Write-Host
    Hdr('CleanTsSearch')

    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        $global:failedBuilds += 'tssearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $tsSearchPath

    Log('npm run clean')
    npm run clean

    $resourcesPath = Join-Path $tsSearchPath 'data'
    CleanJsonResources($resourcesPath)

    Set-Location $oldPwd
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

    PrintFailedBuilds

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

    CleanFsSearch

    CleanGoSearch

    # CleanGroovySearch

    CleanHsSearch

    CleanJavaSearch

    CleanJsSearch

    CleanKtSearch

    CleanObjcSearch

    CleanMlSearch

    CleanPlSearch

    CleanPhpSearch

    CleanPs1Search

    CleanPySearch

    CleanRbSearch

    CleanRsSearch

    CleanScalaSearch

    CleanSwiftSearch

    CleanTsSearch

    PrintFailedBuilds

    exit
}

################################################################################
# Main function
################################################################################

function CleanMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        CleanAll
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'linux'      { CleanLinux }
            # 'bash'       { CleanBashSearch }
            'c'          { CleanCSearch }
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

    PrintFailedBuilds
}

if ($help)
{
    Usage
}

$oldPwd = Get-Location

try {
    if ($all)
    {
        CleanAll
    }

    CleanMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
