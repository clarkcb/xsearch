################################################################################
#
# build.ps1
#
# Builds specified language version of xsearch, or all versions
#
################################################################################
param([string]$lang="all")

########################################
# Configuration
########################################

. $PSScriptRoot\config.ps1
. $PSScriptRoot\common.ps1


########################################
# Utility Functions
########################################

function CopyResources
{
    param($resourcesPath)
    Log("Copy-Item $sharedPath\filetypes.xml -Destination $resourcesPath")
    Copy-Item $sharedPath\filetypes.xml -Destination $resourcesPath
    Log("Copy-Item $sharedPath\searchoptions.xml -Destination $resourcesPath")
    Copy-Item $sharedPath\searchoptions.xml -Destination $resourcesPath
}

function CopyTestResources
{
    param($testResourcesPath)
    Log("Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath")
    Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath
}


################################################################################
# Build functions
################################################################################

function BuildClojure
{
    Write-Host
    Log("BuildClojure - currently unsupported")
}

function BuildCsharp
{
    Write-Host
    Log("BuildCsharp")

    $csharpPath = Join-Path -Path $xsearchPath -ChildPath "csharp"
    $cssearchPath = Join-Path -Path $csharpPath -ChildPath "CsSearch"

    $oldPwd = Get-Location
    Set-Location $cssearchPath

    Log("Building CsSearch solution")
    Write-Host "msbuild CsSearch.sln /t:Rebuild"
    msbuild CsSearch.sln /t:Rebuild

    Set-Location $oldPwd
}

function BuildFsharp
{
    Write-Host
    Log("BuildFsharp - currently unsupported")

    #$fsharpPath = Join-Path -Path $xsearchPath -ChildPath "fsharp"

    #$oldPwd = Get-Location
    #Set-Location $fsharpPath


    ####
    # Getting error: The target "Build" does not exist in the project
    # All found solutions were ineffective
    ####
    #Log("Building FsSearch solution")
    #Write-Host "msbuild FsSearch.sln"
    #msbuild FsSearch.sln

    #Set-Location $oldPwd
}

function BuildGo
{
    Write-Host
    Log("BuildGo")

    Log("Building gengosearchcode")
    Write-Host "go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode"
    go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode

    Log("Running gengosearchcode")
    Write-Host "$ENV:GOPATH\bin\gengosearchcode"
    &$ENV:GOPATH\bin\gengosearchcode

    Log("Auto-formatting gosearch")
    Write-Host "go fmt elocale.com/clarkcb/xsearch"
    go fmt elocale.com/clarkcb/xsearch

    Log("Building gosearch")
    Write-Host "go install elocale.com/clarkcb/xsearch/gosearch"
    go install elocale.com/clarkcb/xsearch/gosearch
}

function BuildHaskell
{
    Write-Host
    Log("BuildHaskell")
    $haskellPath = Join-Path -Path $xsearchPath -ChildPath "haskell"
    $hssearchPath = Join-Path -Path $haskellPath -ChildPath "hssearch"
    $sandboxPath = Join-Path -Path $hssearchPath -ChildPath ".cabal-sandbox"
    $resourcesPath = Join-Path -Path $hssearchPath -ChildPath "data"

    $oldPwd = Get-Location
    Set-Location $hssearchPath

    if (!(Test-Path $sandboxPath))
    {
        Log("Sandbox not found, initializing and installing dependencies")
        Write-Host "cabal sandbox init --sandbox $sandboxPath"
        cabal sandbox init --sandbox $sandboxPath
    }

    if (Test-Path $sandboxPath)
    {
        Log("Installing dependencies")
        Write-Host "cabal install --only-dependencies"
        cabal install --only-dependencies
    }

    if (!(Test-Path $resourcesPath))
    {
        New-Item -Path $hssearchPath -Name "data" -ItemType directory
    }

    # copy the resource files
    Log("Copying the resource files")
    CopyResources($resourcesPath)

    # do the build
    Log("Building hssearch")
    Write-Host "cabal build"
    cabal build

    Set-Location $oldPwd
}

function BuildJava
{
    Write-Host
    Log("BuildJava - currently unsupported")
}

function BuildNode
{
    Write-Host
    Log("BuildNode - currently unsupported")
}

function BuildScala
{
    Write-Host
    Log("BuildScala - currently unsupported")
}

function BuildAll
{
    Write-Host
    Log("BuildAll")

    BuildClojure

    BuildCsharp

    BuildFsharp

    BuildGo

    BuildHaskell

    BuildJava

    BuildNode

    BuildScala
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($lang="all")

    switch ($lang)
    {
        "all"     { BuildAll }
        "clojure" { BuildClojure }
        "csharp"  { BuildCsharp }
        "fsharp"  { BuildFsharp }
        "go"      { BuildGo }
        "haskell" { BuildHaskell }
        "java"    { BuildJava }
        "node"    { BuildNode }
        "scala"   { BuildScala }
        default   { ExitWithError("Unknown option: $lang") }
    }
}

BuildMain $lang
