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
. (Join-Path -Path $xsearchScriptDir -ChildPath 'common.ps1')

if (-not $release)
{
    $debug = $true
}

# args holds the remaining arguments
$langs = $args
$hostname = [System.Net.Dns]::GetHostName()

Hdr('xsearch build script')
Log("user: $env:USER")
Log("host: $hostname")
if ($IsWindows)
{
    Log("os: $env:OS")
}
elseif ($IsLinux)
{
    Log("os: Linux")
}
elseif ($IsMacOS)
{
    Log("os: Darwin")
}
else
{
    Log("os: unknown")
}

$gitBranch = git branch --show-current
$gitCommit = git rev-parse --short HEAD
Log("git branch: $gitBranch ($gitCommit)")

if ($langs -contains 'all')
{
    $all = $true
}

Log("help: $help")
Log("debug: $debug")
Log("release: $release")
Log("venv: $venv")
Log("all: $all")
Log("args: $args")
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
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | lang [lang...]}`n"
    exit
}

function CopySearchOptionsJsonResources
{
    param([string]$resourcesPath)
    $searchOptionsPath = Join-Path $xsearchSharedPath 'searchoptions.json'
    Log("Copy-Item $searchOptionsPath -Destination $resourcesPath")
    Copy-Item $searchOptionsPath -Destination $resourcesPath
}

function CopyTestResources
{
    param([string]$testResourcesPath)

    $testFiles = @(Get-ChildItem -Path $xsearchTestFilePath -File) |
                   Where-Object { $_.Name.StartsWith('testFile') -and $_.Extension -eq '.txt' }
    foreach ($testFile in $testFiles)
    {
        Log("Copy-Item $testFile -Destination $testResourcesPath")
        Copy-Item $testFile -Destination $testResourcesPath
    }
}

function AddSoftLink
{
    param([string]$linkPath, [string]$targetPath, [bool]$replaceLink=$true)
    # Write-Host "linkPath: $linkPath"
    # Write-Host "targetPath: $targetPath"

    if ((Test-Path $linkPath) -and $replaceLink)
    {
        if ((Get-Item $linkPath).LinkType -eq 'SymbolicLink')
        {
            Log("Remove-Item $linkPath")
            Remove-Item $linkPath
        }
    }

    if (-not (Test-Path $linkPath))
    {
        # from https://winaero.com/create-symbolic-link-windows-10-powershell/
        # New-Item -ItemType SymbolicLink -Path "Link" -Target "Target"
        Log("New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath")
        New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath
    }
}

function AddToBin
{
    param([string]$xsearchScriptPath)

    if (-not (Test-Path $xsearchBinPath))
    {
        New-Item -ItemType directory -Path $xsearchBinPath
    }

    # get the base filename, minus path and any extension
    $baseName = [io.path]::GetFileNameWithoutExtension($xsearchScriptPath)
    if ($baseName.EndsWith('.debug') -or $baseName.EndsWith('.release'))
    {
        $baseName = $baseName.Split('.')[0]
    }

    $linkPath = Join-Path $xsearchBinPath $baseName

    AddSoftLink $linkPath $xsearchScriptPath
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
# Build functions
################################################################################

function BuildBashSearch
{
    Write-Host
    Hdr('BuildBashSearch')
    Log("language: bash")

    Log("Not currently implemented")
}

function BuildCSearch
{
    Write-Host
    Hdr('BuildCSearch')
    Log("language: C")

    if ($IsWindows)
    {
        Log('BuildCFind - currently unimplemented for Windows')
        return
    }
    if (!$IsMacOS -and !$IsLinux)
    {
        Log('Skipping for unknown/unsupported OS')
        return
    }

    # ensure make is installed
    if (-not (Get-Command 'make' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install make')
        $global:failedBuilds += 'csearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $cSearchPath

    Log('Building csearch')
    Log('make')
    make

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'csearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $cSearchExe = Join-Path $cSearchPath 'csearch'
    AddToBin($cSearchExe)

    Set-Location $oldPwd
}

function BuildCljSearch
{
    Write-Host
    Hdr('BuildCljSearch')
    Log("language: clojure")

    # ensure clojure is installed
    if (-not (Get-Command 'clj' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install clojure')
        $global:failedBuilds += 'cljsearch'
        return
    }

    # clj -version output looks like this: Clojure CLI version 1.11.4.1474
    $clojureVersion = clj -version 2>&1
    Log("clojure version: $clojureVersion")

    # ensure leiningen is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        $global:failedBuilds += 'cljsearch'
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $cljSearchPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $cljSearchPath

    # Create uberjar with lein
    Log('Building cljsearch')
    Log('lein clean')
    lein clean
    Log('lein uberjar')
    lein uberjar

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'cljsearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $cljSearchExe = Join-Path $cljSearchPath 'bin' 'cljsearch.ps1'
    AddToBin($cljSearchExe)

    Set-Location $oldPwd
}

function BuildCppSearch
{
    Write-Host
    Hdr('BuildCppSearch')
    Log("language: C++")

    if ($IsWindows)
    {
        Log('BuildCppSearch - currently unimplemented for Windows')
        return
    }
    if (!$IsMacOS -and !$IsLinux)
    {
        Log('Skipping for unknown/unsupported OS')
        return
    }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        $global:failedBuilds += 'cppsearch'
        return
    }

    # cmake --version output looks like this: cmake version 3.30.2
    $cmakeVersion = cmake --version
    $cmakeVersion = @($cmakeVersion -split '\s+')[2]
    Log("cmake version: $cmakeVersion")

    $oldPwd = Get-Location
    Set-Location $cppSearchPath

    # Set CMAKE_CXX_FLAGS
    $cmakeCxxFlags = "-W -Wall -Werror -Wextra -Wshadow -Wnon-virtual-dtor -pedantic"

    # Add AddressSanitizer
    # $cmakeCxxFlags = "$cmakeCxxFlags -fsanitize=address -fno-omit-frame-pointer"

    $configurations = @()
    if ($debug)
    {
        $configurations += 'debug'
    }
    if ($release)
    {
        $configurations += 'release'
    }
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = "cmake-build-$c"
        $cmakeBuildPath = Join-Path $cppSearchPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath))
        {
            New-Item -ItemType directory -Path $cmakeBuildPath

            Set-Location $cmakeBuildPath

            Log("cmake -G ""Unix Makefiles"" -DCMAKE_BUILD_TYPE=$c ..")
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # Log("make -f Makefile")
            # make -f Makefile

            Set-Location $cppSearchPath
        }

        $targets = @('clean', 'cppsearch', 'cppsearchapp', 'cppsearch-tests')
        ForEach ($t in $targets)
        {
            Log("cmake --build $cmakeBuildDir --config $c --target $t -- $cmakeCxxFlags")
            cmake --build $cmakeBuildDir --config $c --target $t -- $cmakeCxxFlags
            if ($LASTEXITCODE -eq 0)
            {
                Log("Build target $t succeeded")
            }
            else
            {
                PrintError("Build target $t failed")
                $global:failedBuilds += 'cppsearch'
                Set-Location $oldPwd
                return
            }
        }
    }

    if ($release)
    {
        # add release to bin
        $cppSearchExe = Join-Path $cppSearchPath 'bin' 'cppsearch.release.ps1'
        AddToBin($cppSearchExe)
    }
    else
    {
        # add debug to bin
        $cppSearchExe = Join-Path $cppSearchPath 'bin' 'cppsearch.debug.ps1'
        AddToBin($cppSearchExe)
    }

    Set-Location $oldPwd
}

function BuildCsSearch
{
    Write-Host
    Hdr('BuildCsSearch')
    Log("language: C#")

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'cssearch'
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $resourcesPath = Join-Path $csSearchPath 'CsSearchLib' 'Resources'
    $testResourcesPath = Join-Path $csSearchPath 'CsSearchTests' 'Resources'

    # copy the shared json files to the local resource location
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    # copy the shared test files to the local test resource location
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $csSearchPath

    $csSearchSolutionPath = Join-Path $csSearchPath 'CsSearch.sln'

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    # run dotnet build selected configurations
    ForEach ($c in $configurations)
    {
        Log("Building CsSearch solution for $c configuration")
        Log("dotnet build $csSearchSolutionPath --configuration $c")
        dotnet build $csSearchSolutionPath --configuration $c

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'cssearch'
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        # add release to bin
        $csSearchExe = Join-Path $csSearchPath 'bin' 'cssearch.release.ps1'
        AddToBin($csSearchExe)
    }
    else
    {
        # add debug to bin
        $csSearchExe = Join-Path $csSearchPath 'bin' 'cssearch.debug.ps1'
        AddToBin($csSearchExe)
    }

    Set-Location $oldPwd
}

function BuildDartSearch
{
    Write-Host
    Hdr('BuildDartSearch')
    Log("language: dart")

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        $global:failedBuilds += 'dartsearch'
        return
    }

    $dartVersion = dart --version
    Log("$dartVersion")

    $oldPwd = Get-Location
    Set-Location $dartSearchPath

    Log('Building dartsearch')
    if ((-not (Test-Path (Join-Path $dartSearchPath '.dart_tool' 'package_config.json'))) -and
        (-not (Test-Path (Join-Path $dartSearchPath '.packages'))))
    {
        Log('dart pub get')
        dart pub get
    }
    else
    {
        Log('dart pub upgrade')
        dart pub upgrade
    }

    Log('Compiling dartsearch')
    $dartsearchDart = Join-Path $dartSearchPath 'bin' 'dartsearch.dart'
    Log("dart compile exe $dartsearchDart")
    dart compile exe $dartsearchDart

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'dartsearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $dartSearchExe = Join-Path $dartSearchPath 'bin' 'dartsearch.ps1'
    AddToBin($dartSearchExe)

    Set-Location $oldPwd
}

function BuildExSearch
{
    Write-Host
    Hdr('BuildExSearch')
    Log("language: elixir")

    # ensure elixir is installed
    if (-not (Get-Command 'elixir' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install elixir')
        $global:failedBuilds += 'exsearch'
        return
    }

    $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
    Log("elixir version: $elixirVersion")

    # ensure mix is installed
    if (-not (Get-Command 'mix' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mix')
        $global:failedBuilds += 'exsearch'
        return
    }

    $mixVersion = mix --version | Select-String -Pattern 'Mix'
    Log("mix version: $mixVersion")

    $oldPwd = Get-Location
    Set-Location $exSearchPath

    Log('Getting exsearch dependencies')
    Log('mix deps.get')
    mix deps.get

    Log('Compiling exsearch')
    Log('mix compile')
    mix compile

    Log('Creating exsearch executable')
    Log('mix escript.build')
    mix escript.build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'exsearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $exSearchExe = Join-Path $exSearchPath 'bin' 'exsearch'
    AddToBin($exSearchExe)

    Set-Location $oldPwd
}

function BuildFsSearch
{
    Write-Host
    Hdr('BuildFsSearch')
    Log("language: F#")

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'fssearch'
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $resourcesPath = Join-Path $fsSearchPath 'FsSearchLib' 'Resources'
    $testResourcesPath = Join-Path $fsSearchPath 'FsSearchTests' 'Resources'

    # copy the shared json files to the local resource location
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    # copy the shared test files to the local test resource location
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $fsSearchPath

    $fsSearchSolutionPath = Join-Path $fsSearchPath 'FsSearch.sln'

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    # run dotnet build for selected configurations
    ForEach ($c in $configurations)
    {
        Log("Building FsSearch solution for $c configuration")
        Log("dotnet build $fsSearchSolutionPath --configuration $c")
        dotnet build $fsSearchSolutionPath --configuration $c

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'fssearch'
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        # add release to bin
        $fsSearchExe = Join-Path $fsSearchPath 'bin' 'fssearch.release.ps1'
        AddToBin($fsSearchExe)
    }
    else
    {
        # add debug to bin
        $fsSearchExe = Join-Path $fsSearchPath 'bin' 'fssearch.debug.ps1'
        AddToBin($fsSearchExe)
    }

    Set-Location $oldPwd
}

function BuildGoSearch
{
    Write-Host
    Hdr('BuildGoSearch')
    Log("language: go")

    # ensure go is installed
    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        $global:failedBuilds += 'gosearch'
        return
    }

    $goVersion = (go version) -replace 'go version ', ''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $goSearchPath

    # go fmt the gosearch source (for auto-generated code)
    Log('Auto-formatting gosearch')
    Log('go fmt ./...')
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if (-not (Test-Path $xsearchBinPath))
    {
        New-Item -ItemType directory -Path $xsearchBinPath
    }

    # if GOBIN not defined, set to BIN_PATH
    if (-not (Test-Path Env:GOBIN))
    {
        $env:GOBIN = $xsearchBinPath
    }

    # now build gosearch
    Log('Building gosearch')
    Log('go install ./...')
    go install ./...

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'gosearch'
        Set-Location $oldPwd
        return
    }

    if ($env:GOBIN -ne $xsearchBinPath)
    {
        # add to bin
        $goSearchExe = Join-Path $env:GOBIN 'gosearch'
        AddToBin($goSearchExe)
    }

    Set-Location $oldPwd
}

function BuildGroovySearch
{
    Write-Host
    Hdr('BuildGroovySearch')
    Log("language: groovy")

    Log("Not currently implemented")
}

function BuildHsSearch
{
    Write-Host
    Hdr('BuildHsSearch')
    Log("language: haskell")

    # ensure ghc is installed
    if (-not (Get-Command 'ghc' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ghc')
        $global:failedBuilds += 'hssearch'
        return
    }

    $ghcVersion = ghc --version
    Log("ghc version: $ghcVersion")

    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        $global:failedBuilds += 'hssearch'
        return
    }

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    # set the default stack settings, e.g. use system ghc
    $stackDir = Join-Path $HOME '.stack'
    if (-not (Test-Path $stackDir))
    {
        New-Item -ItemType directory -Path $stackDir
    }
    $configYaml = Join-Path $stackDir 'config.yaml'
    if (-not (Test-Path $configYaml))
    {
        New-Item -ItemType file -Path $stackDir -Name "config.yaml" -Value "install-ghc: false`nsystem-ghc: true"
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $hsSearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $hsSearchPath

    # build with stack (via make)
    Log('Building hssearch')
    Log('stack setup')
    make setup

    Log('stack build')
    make build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'hssearch'
        Set-Location $oldPwd
        return
    }

    Log("stack install --local-bin-path $xsearchBinPath")
    stack install --local-bin-path $xsearchBinPath

    Set-Location $oldPwd
}

function BuildJavaSearch
{
    Write-Host
    Hdr('BuildJavaSearch')
    Log("language: java")

    # ensure java is installed
    if (-not (Get-Command 'java' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install java')
        $global:failedBuilds += 'javasearch'
        return
    }

    $javaVersion = java -version 2>&1 | Select-String -Pattern 'version'
    Log("java version: $javaVersion")

    # ensure mvn is installed
    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install maven')
        $global:failedBuilds += 'javasearch'
        return
    }

    $mvnVersion = mvn --version 2>&1 | Select-String -Pattern 'Apache Maven'
    Log("mvn version: $mvnVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $javaSearchPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $javaSearchPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run maven clean package (skip testing as this is run via unittest.sh)
    Log('Building javasearch')
    Log("mvn -f $javaSearchPath/pom.xml clean package -Dmaven.test.skip=true -Dmaven.plugin.validation=DEFAULT")
    mvn -f $javaSearchPath/pom.xml clean package '-Dmaven.test.skip=true' '-Dmaven.plugin.validation=DEFAULT'

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'javasearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $javaSearchExe = Join-Path $javaSearchPath 'bin' 'javasearch.ps1'
    AddToBin($javaSearchExe)
}

function BuildJsSearch
{
    Write-Host
    Hdr('BuildJsSearch')
    Log("language: javascript")

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js')
        $global:failedBuilds += 'jssearch'
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        $global:failedBuilds += 'jssearch'
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $jsSearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $jsSearchPath

    # run npm install and build
    Log('Building jssearch')
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'jssearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $jsSearchExe = Join-Path $jsSearchPath 'bin' 'jssearch.ps1'
    AddToBin($jsSearchExe)

    Set-Location $oldPwd
}

function BuildKtSearch
{
    Write-Host
    Hdr('BuildKtSearch')
    Log("language: kotlin")

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
        return
    }

    $gradleOutput = & $gradle --version

    $gradleVersion = $gradleOutput | Where-Object {$_.Contains('Gradle')} | ForEach-Object {$_ -replace 'Gradle\s+',''}
    Log("$gradle version: $gradleVersion")

    $kotlinVersion = $gradleOutput | Where-Object {$_.Contains('Kotlin')} | ForEach-Object {$_ -replace 'Kotlin:\s+',''}
    Log("Kotlin version: $kotlinVersion")

    $jvmVersion = $gradleOutput | Where-Object {$_.Contains('Launcher')} | ForEach-Object {$_ -replace 'Launcher JVM:\s+',''}
    Log("JVM version: $jvmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $ktSearchPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $ktSearchPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # TEMP(?): copy the jar file for the local ktfind dependency to lib
    $ktfindLibDir = Join-Path $env:XFIND_PATH 'kotlin' 'ktfind' 'build' 'libs'
    $ktFindJars = @(Get-ChildItem -Path $ktfindLibDir -File) |
                    Where-Object { $_.Name.StartsWith('ktfind') }
    if ($ktFindJars.count -gt 0)
    {
        $ktFindJar = $ktFindJars[0]
        $ktSearchLibPath = Join-Path $ktSearchPath 'lib'
        Log("Copy-Item $ktFindJar -Destination $ktSearchLibPath")
        Copy-Item $ktFindJar -Destination $ktSearchLibPath
    }

    # run a gradle build
    Log('Building ktsearch')
    Log("$gradle --warning-mode all clean jar")
    & $gradle --warning-mode all clean jar

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'ktsearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $ktSearchExe = Join-Path $ktSearchPath 'bin' 'ktsearch.ps1'
    AddToBin($ktSearchExe)

    Set-Location $oldPwd
}

function BuildObjcSearch
{
    Write-Host
    Hdr('BuildObjcSearch')
    Log("language: objc")

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'objcsearch'
        return
    }

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Apple Swift'
    $swiftVersion = @($swiftVersion -split '\s+')[3]
    Log("swift version: Apple Swift version $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $objcSearchPath

    if ($debug)
    {
        Log("swift build")
        swift build

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'objcsearch'
            Set-Location $oldPwd
            return
        }
    }
    if ($release)
    {
        Log("swift build --configuration release")
        swift build --configuration release

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'objcsearch'
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $objcSearchExe = Join-Path $objcSearchPath 'bin' 'objcsearch.release.ps1'
        AddToBin($objcSearchExe)
    }
    else
    {
        # add debug to bin
        $objcSearchExe = Join-Path $objcSearchPath 'bin' 'objcsearch.debug.ps1'
        AddToBin($objcSearchExe)
    }

    Set-Location $oldPwd
}

function BuildMlSearch
{
    Write-Host
    Hdr('BuildMlSearch')
    Log("language: ocaml")

    Log("Not currently implemented")
}

function BuildPlSearch
{
    Write-Host
    Hdr('BuildPlSearch')
    Log("language: perl")

    # ensure perl is installed
    if (-not (Get-Command 'perl' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install perl')
        $global:failedBuilds += 'plsearch'
        return
    }

    $perlVersion = perl -e 'print $^V' | Select-String -Pattern 'v5'
    if (-not $perlVersion)
    {
        PrintError('A 5.x version of perl is required')
        $global:failedBuilds += 'plsearch'
        return
    }

    Log("perl version: $perlVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $plSearchPath 'share'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'plsearch'
        return
    }

    # add to bin
    $plSearchExe = Join-Path $plSearchPath 'bin' 'plsearch.ps1'
    AddToBin($plSearchExe)
}

function BuildPhpSearch
{
    Write-Host
    Hdr('BuildPhpSearch')
    Log("language: php")

    # ensure php is installed
    if (-not (Get-Command 'php' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install php')
        $global:failedBuilds += 'phpsearch'
        return
    }

    $phpVersion = & php -v | Select-String -Pattern '^PHP [78]' 2>&1
    if (-not $phpVersion)
    {
        PrintError('A version of PHP >= 7.x is required')
        $global:failedBuilds += 'phpsearch'
        return
    }
    Log("php version: $phpVersion")

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install composer')
        $global:failedBuilds += 'phpsearch'
        return
    }

    $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
    Log("composer version: $composerVersion")

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $xsearchSharedPath 'config.json'
    $configPath = Join-Path $phpSearchPath 'config'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $phpSearchPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $phpSearchPath

    # run a composer build
    Log('Building phpsearch')

    if (Test-Path (Join-Path $phpSearchPath 'vendor'))
    {
        Log('composer update')
        composer update
    }
    else
    {
        Log('composer install')
        composer install
    }

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'phpsearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $phpSearchExe = Join-Path $phpSearchPath 'bin' 'phpsearch.ps1'
    AddToBin($phpSearchExe)

    Set-Location $oldPwd
}

function BuildPs1Search
{
    Write-Host
    Hdr('BuildPs1Search')
    Log("language: powershell")

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $oldPwd = Get-Location
    Set-Location $ps1FindPath

    Log('Building ps1search')

    # copy the file to the first of the module paths, if defined
    $modulePaths = @($env:PSModulePath -split ':')
    if ($modulePaths.Count -gt 0) {
        $ps1SearchTargetModulePath = Join-Path $modulePaths[0] 'Ps1SearchModule'
        if (-not (Test-Path $ps1SearchTargetModulePath)) {
            Log("New-Item -Path $ps1SearchTargetModulePath -ItemType Directory")
            New-Item -Path $ps1SearchTargetModulePath -ItemType Directory
        }
        $ps1fSearchModulePath = Join-Path $ps1SearchPath 'Ps1SearchModule.psm1'
        Log("Copy-Item $ps1fSearchModulePath -Destination $ps1SearchTargetModulePath")
        Copy-Item $ps1fSearchModulePath -Destination $ps1SearchTargetModulePath
    }

    # add to bin
    $ps1SearchExe = Join-Path $ps1SearchPath 'ps1search.ps1'
    AddToBin($ps1SearchExe)

    Set-Location $oldPwd
}

function BuildPySearch
{
    Write-Host
    Hdr('BuildPySearch')
    Log("language: python")

    $oldPwd = Get-Location
    Set-Location $pySearchPath

    # Set to $true to use venv
    $useVenv=$venv
    # $pythonVersions = @('python3.12', 'python3.11', 'python3.10', 'python3.9')
    # We don't want to use python3.12 yet
    $pythonVersions = @('python3.11', 'python3.10', 'python3.9')
    $python = ''
    $venvPath = Join-Path $pySearchPath 'venv'

    $activeVenv = ''

    if ($useVenv)
    {
        Log('Using venv')

        # 3 possibilities:
        # 1. venv exists and is active
        # 2. venv exists and is not active
        # 3. venv does not exist

        if ($env:VIRTUAL_ENV)
        {
            # 1. venv exists and is active
            Log("Already active venv: $env:VIRTUAL_ENV")
            $activeVenv = $env:VIRTUAL_ENV

            ForEach ($p in $pythonVersions)
            {
                $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
                if ($null -ne $pythonCmd)
                {
                    $python = $p
                    break
                }
            }
        }
        elseif (Test-Path $venvPath)
        {
            # 2. venv exists and is not active
            Log('Using existing venv')

            # activate the venv
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log("$activatePath")
            & $activatePath

            ForEach ($p in $pythonVersions)
            {
                $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
                if ($null -ne $pythonCmd)
                {
                    $python = $p
                    break
                }
            }
        }
        else
        {
            # 3. venv does not exist
            # ensure python3.9+ is installed
            ForEach ($p in $pythonVersions)
            {
                $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
                if ($null -ne $pythonCmd)
                {
                    $python = $p
                    break
                }
            }

            if (-not $python)
            {
                PrintError('You need to install python(>= 3.9)')
                return
            }

            Log('Creating new venv')

            # create a virtual env to run from and install to
            Log("$python -m venv venv")
            & $python -m venv venv

            # activate the virtual env
            $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
            Log("$activatePath")
            & $activatePath
        }
    }
    else
    {
        Log('Not using venv')

        # ensure python3.9+ is installed
        ForEach ($p in $pythonVersions)
        {
            $pythonCmd = Get-Command $p -ErrorAction 'SilentlyContinue'
            if ($null -ne $pythonCmd)
            {
                $python = $p
                break
            }
        }

        if (-not $python)
        {
            PrintError('You need to install python(>= 3.9)')
            return
        }
    }

    $pythonExePath = Get-Command $python | Select-Object -ExpandProperty Source
    Log("Using $python ($pythonExePath)")
    $pythonVersion = & $python -V | Select-String -Pattern '^Python'
    Log("Version: $pythonVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $pySearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)


    # install dependencies in requirements.txt
    Log('pip3 install -r requirements.txt')
    pip3 install -r requirements.txt

    # check for success/failure
    $buildError = $false
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'pysearch'
        $buildError = $true
    }

    # if there was not an active venv before the build, deactivate the venv
    if ($useVenv -and -not $activeVenv)
    {
        # deactivate at end of setup process
        Log('deactivate')
        deactivate
    }

    if ($buildError)
    {
        Set-Location $oldPwd
        return
    }

    # add to bin
    $pySearchExe = Join-Path $pySearchPath 'bin' 'pysearch.ps1'
    AddToBin($pySearchExe)

    Set-Location $oldPwd
}

function BuildRbSearch
{
    Write-Host
    Hdr('BuildRbSearch')
    Log("language: ruby")

    # ensure ruby2.x is installed
    if (-not (Get-Command 'ruby' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ruby')
        $global:failedBuilds += 'rbsearch'
        return
    }

    $rubyVersion = & ruby -v 2>&1 | Select-String -Pattern '^ruby 3' 2>&1
    if (-not $rubyVersion)
    {
        PrintError('A version of ruby >= 3.x is required')
        $global:failedBuilds += 'rbsearch'
        return
    }
    Log("ruby version: $rubyVersion")

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $xsearchSharedPath 'config.json'
    $configPath = Join-Path $rbSearchPath 'data'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $rbSearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $rbSearchPath

    Log('Building rbsearch')
    Log('bundle install')
    bundle install

    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'rbsearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $rbSearchExe = Join-Path $rbSearchPath 'bin' 'rbsearch.ps1'
    AddToBin($rbSearchExe)

    Set-Location $oldPwd
}

function BuildRsSearch
{
    Write-Host
    Hdr('BuildRsSearch')
    Log("language: rust")

    # ensure rust is installed
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rust')
        $global:failedBuilds += 'rssearch'
        return
    }

    $rustVersion = rustc --version | Select-String -Pattern 'rustc'
    Log("rustc version: $rustVersion")

    # ensure cargo is installed
    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        $global:failedBuilds += 'rssearch'
        return
    }

    $cargoVersion = cargo --version | Select-String -Pattern 'cargo'
    Log("cargo version: $cargoVersion")

    $oldPwd = Get-Location
    Set-Location $rsSearchPath

    Log('Building rssearch')

    if ($debug)
    {
        Log('cargo build')
        cargo build

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'rssearch'
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        Log('cargo build --release')
        cargo build --release

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'rssearch'
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $rsSearchExe = Join-Path $rsSearchPath 'bin' 'rssearch.release.ps1'
        AddToBin($rsSearchExe)
    }
    else
    {
        # add debug to bin
        $rsSearchExe = Join-Path $rsSearchPath 'bin' 'rssearch.debug.ps1'
        AddToBin($rsSearchExe)
    }

    Set-Location $oldPwd
}

function BuildScalaSearch
{
    Write-Host
    Hdr('BuildScalaSearch')
    Log("language: scala")

    # ensure sbt is installed
    if (-not (Get-Command 'scala' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala')
        $global:failedBuilds += 'scalasearch'
        return
    }

    # scala --version output looks like this:
    # Scala code runner version: 1.4.3
    # Scala version (default): 3.5.2
    $scalaVersion = scala --version 2>&1 | Select-Object -Last 1
    $scalaVersion = @($scalaVersion -split '\s+')[3]
    Log("scala version: $scalaVersion")

    $oldPwd = Get-Location
    Set-Location $scalaSearchPath

    # ensure sbt is installed
    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install sbt')
        $global:failedBuilds += 'scalasearch'
        return
    }

    $sbtOutput = sbt --version

    $sbtProjectVersion = $sbtOutput | Select-String -Pattern 'project'
    Log("$sbtProjectVersion")

    $sbtScriptVersion = $sbtOutput | Select-String -Pattern 'script'
    Log("$sbtScriptVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $scalaSearchPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $scalaSearchPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $scalaVersion = '3.5.2'
    if (Test-Path Env:SCALA_VERSION)
    {
        $scalaVersion = $env:SCALA_VERSION
    }

    # TEMP(?): copy the jar file for the local scalafind dependency to lib
    $scalafindLibDir = Join-Path "$env:XFIND_PATH" 'scala' 'scalafind' 'target' "scala-$scalaVersion"
    $scalaFindJars = @(Get-ChildItem -Path $scalafindLibDir -File) |
                    Where-Object { $_.Name.StartsWith('scalafind') -and $_.Name.EndsWith('.jar') } |
                    Where-Object { -not $_.Name.Contains('assembly') }
    if ($scalaFindJars.count -gt 0)
    {
        $scalaFindJar = $scalaFindJars[0]
        $scalaSearchLibPath = Join-Path $scalaSearchPath 'lib'
        Log("Copy-Item $scalaFindJar -Destination $scalaSearchLibPath")
        Copy-Item $scalaFindJar -Destination $scalaSearchLibPath
    }

    # run sbt assembly
    Log('Building scalasearch')
    Log("sbt 'set test in assembly := {}' clean assembly")
    sbt 'set test in assembly := {}' clean assembly

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'scalasearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $scalaSearchExe = Join-Path $scalaSearchPath 'bin' 'scalasearch.ps1'
    AddToBin($scalaSearchExe)

    Set-Location $oldPwd
}

function BuildSwiftSearch
{
    Write-Host
    Hdr('BuildSwiftSearch')
    Log("language: swift")

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'swiftsearch'
        return
    }

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Apple Swift'
    $swiftVersion = @($swiftVersion -split '\s+')[3]
    Log("swift version: Apple Swift version $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftSearchPath

    Log('Building swiftsearch')

    if ($debug)
    {
        Log('swift build')
        swift build

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'swiftsearch'
            Set-Location $oldPwd
            return
        }
    }

    if ($release)
    {
        Log('swift build --configuration release')
        swift build --configuration release

        # check for success/failure
        if ($LASTEXITCODE -eq 0)
        {
            Log('Build succeeded')
        }
        else
        {
            PrintError('Build failed')
            $global:failedBuilds += 'swiftsearch'
            Set-Location $oldPwd
            return
        }

        # add release to bin
        $swiftSearchExe = Join-Path $swiftSearchPath 'bin' 'swiftsearch.release.ps1'
        AddToBin($swiftSearchExe)
    }
    else
    {
        # add debug to bin
        $swiftSearchExe = Join-Path $swiftSearchPath 'bin' 'swiftsearch.debug.ps1'
        AddToBin($swiftSearchExe)
    }

    Set-Location $oldPwd
}

function BuildTsSearch
{
    Write-Host
    Hdr('BuildTsSearch')
    Log("language: typescript")

    # ensure node is installed
    if (-not (Get-Command 'node' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js')
        $global:failedBuilds += 'tssearch'
        return
    }

    $nodeVersion = node --version
    Log("node version: $nodeVersion")

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        $global:failedBuilds += 'tssearch'
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $tsSearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopySearchOptionsJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $tsSearchPath

    # run npm install and build
    Log('Building tssearch')
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Build succeeded')
    }
    else
    {
        PrintError('Build failed')
        $global:failedBuilds += 'tssearch'
        Set-Location $oldPwd
        return
    }

    # add to bin
    $tsSearchExe = Join-Path $tsSearchPath 'bin' 'tssearch.ps1'
    AddToBin($tsSearchExe)

    Set-Location $oldPwd
}

function BuildLinux
{
    Write-Host
    Hdr('BuildLinux')

    Measure-Command { BuildBashSearch }

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

    PrintFailedBuilds

    exit
}

function BuildAll
{
    Write-Host
    Hdr('BuildAll')

    Measure-Command { BuildBashSearch }

    # Measure-Command { BuildCSearch }

    Measure-Command { BuildCljSearch }

    Measure-Command { BuildCppSearch }

    Measure-Command { BuildCsSearch }

    Measure-Command { BuildDartSearch }

    Measure-Command { BuildExSearch }

    Measure-Command { BuildFsSearch }

    Measure-Command { BuildGoSearch }

    Measure-Command { BuildGroovySearch }

    Measure-Command { BuildHsSearch }

    Measure-Command { BuildJavaSearch }

    Measure-Command { BuildJsSearch }

    Measure-Command { BuildKtSearch }

    Measure-Command { BuildObjcSearch }

    Measure-Command { BuildMlSearch }

    Measure-Command { BuildPlSearch }

    Measure-Command { BuildPhpSearch }

    Measure-Command { BuildPs1Search }

    Measure-Command { BuildPySearch }

    Measure-Command { BuildRbSearch }

    Measure-Command { BuildRsSearch }

    Measure-Command { BuildScalaSearch }

    Measure-Command { BuildSwiftSearch }

    Measure-Command { BuildTsSearch }

    PrintFailedBuilds

    exit
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        BuildAll
        exit
    }
    if ($langs -contains 'linux')
    {
        BuildLinux
        exit
    }

    ForEach ($lang in $langs)
    {
        switch ($lang.ToLower())
        {
            'bash'       { Measure-Command { BuildBashSearch } }
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
            'groovy'     { Measure-Command { BuildGroovySearch } }
            'haskell'    { Measure-Command { BuildHsSearch } }
            'hs'         { Measure-Command { BuildHsSearch } }
            'java'       { Measure-Command { BuildJavaSearch } }
            'javascript' { Measure-Command { BuildJsSearch } }
            'js'         { Measure-Command { BuildJsSearch } }
            'kotlin'     { Measure-Command { BuildKtSearch } }
            'kt'         { Measure-Command { BuildKtSearch } }
            'objc'       { Measure-Command { BuildObjcSearch } }
            'ocaml'      { Measure-Command { BuildMlSearch } }
            'ml'         { Measure-Command { BuildMlSearch } }
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
        BuildAll
    }

    BuildMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
