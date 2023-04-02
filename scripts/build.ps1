#!/usr/bin/env pwsh
################################################################################
#
# build.ps1
#
# Builds specified language version of xsearch, or all versions
#
################################################################################
param([switch]$help = $false,
      [switch]$debug = $false,
      [switch]$release = $false,
      [switch]$venv = $false,
      [string]$lang = '')

########################################
# Configuration
########################################

$scriptPath = $MyInvocation.MyCommand.Path
$scriptDir = Split-Path $scriptPath -Parent

. (Join-Path -Path $scriptDir -ChildPath 'config.ps1')
. (Join-Path -Path $scriptDir -ChildPath 'common.ps1')

# check for help switch
$help = $help.IsPresent

# for languages that have debug and release builds
$debug = $debug.IsPresent
$release = $release.IsPresent
if (-not $release)
{
    $debug = $true
}

# for python
$venv = $venv.IsPresent


########################################
# Utility Functions
########################################

function Usage
{
    Write-Host "`nUsage: build.ps1 [-help] [-debug] [-release] [-venv] {""all"" | langcode}`n"
    exit
}

function CopyJsonResources
{
    param([string]$resourcesPath)
    $fileTypesPath = Join-Path $sharedPath 'filetypes.json'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $searchOptionsPath = Join-Path $sharedPath 'searchoptions.json'
    Log("Copy-Item $searchOptionsPath -Destination $resourcesPath")
    Copy-Item $searchOptionsPath -Destination $resourcesPath
}

function CopyXmlResources
{
    param([string]$resourcesPath)
    $fileTypesPath = Join-Path $sharedPath 'filetypes.xml'
    Log("Copy-Item $fileTypesPath -Destination $resourcesPath")
    Copy-Item $fileTypesPath -Destination $resourcesPath
    $searchOptionsPath = Join-Path $sharedPath 'searchoptions.xml'
    Log("Copy-Item $searchOptionsPath -Destination $resourcesPath")
    Copy-Item $searchOptionsPath -Destination $resourcesPath
}

function CopyTestResources
{
    param([string]$testResourcesPath)
    Log("Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath")
    Copy-Item $testFilePath -Include testFile*.txt -Destination $testResourcesPath
}

function AddSoftLink
{
    param([string]$linkPath, [string]$targetPath, [bool]$replaceLink=$true)
    Write-Host "linkPath: $linkPath"
    Write-Host "targetPath: $targetPath"

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
    param([string]$scriptPath)

    if (-not (Test-Path $binPath))
    {
        New-Item -ItemType directory -Path $binPath
    }

    # get the base filename, minus path and any extension
    $baseName = [io.path]::GetFileNameWithoutExtension($scriptPath)
    if ($baseName.EndsWith(".debug") -or $baseName.EndsWith(".release"))
    {
        $baseName = $baseName.Split(".")[0]
    }

    $linkPath = Join-Path $binPath $baseName

    AddSoftLink $linkPath $scriptPath
}


################################################################################
# Build functions
################################################################################

function BuildC
{
    Write-Host
    Hdr('BuildC')

    # ensure make is installed
    if (-not (Get-Command 'make' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install make')
        return
    }

    $oldPwd = Get-Location
    Set-Location $csearchPath

    Log('Building csearch')
    Log('make')
    make

    # add to bin
    $csearchExe = Join-Path $csearchPath 'csearch'
    AddToBin($csearchExe)

    Set-Location $oldPwd
}

function BuildClojure
{
    Write-Host
    Hdr('BuildClojure')

    # ensure leiningen is installed
    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $cljsearchPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $cljsearchPath

    # Create uberjar with lein
    Log('Building cljsearch')
    Log('lein clean')
    lein clean
    Log('lein uberjar')
    lein uberjar

    # add to bin
    $cljsearchExe = Join-Path $cljsearchPath 'bin' 'cljsearch.ps1'
    AddToBin($cljsearchExe)

    Set-Location $oldPwd
}

function BuildCpp
{
    Write-Host
    Hdr('BuildCpp')

    if ($IsWindows)
    {
        Log('BuildCpp - currently unimplemented for Windows')
        return
    }
    if (!$IsMacOS -and !$IsLinux)
    {
        Log('Skipping for unknown OS')
        return
    }

    # ensure cmake is installed
    if (-not (Get-Command 'cmake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cmake')
        return
    }

    $oldPwd = Get-Location
    Set-Location $cppsearchPath

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
        $cmakeBuildPath = Join-Path $cppsearchPath $cmakeBuildDir

        if (-not (Test-Path $cmakeBuildPath))
        {
            New-Item -ItemType directory -Path $cmakeBuildPath

            Set-Location $cmakeBuildPath

            Log('cmake -G "Unix Makefiles" ..')
            cmake -G "Unix Makefiles" ..

            Log("make -f Makefile")
            make -f Makefile

            Set-Location $cppsearchPath
        }

        $targets = @('clean', 'cppsearch', 'cppsearch-tests')
        ForEach ($t in $targets)
        {
            Log("cmake --build $cmakeBuildDir --target $t -- -W -Wall -Werror")
            cmake --build $cmakeBuildDir --target $t -- -W -Wall -Werror
            if ($LASTEXITCODE -ne 0)
            {
                Log("An error occurred while trying to run build target $t")
                exit
            }
        }
    }

    if ($release)
    {
        # add release to bin
        $cppsearchExe = Join-Path $cppsearchPath 'bin' 'cppsearch.release.ps1'
        AddToBin($cppsearchExe)
    }
    else
    {
        # add debug to bin
        $cppsearchExe = Join-Path $cppsearchPath 'bin' 'cppsearch.debug.ps1'
        AddToBin($cppsearchExe)
    }

    Set-Location $oldPwd
}

function BuildCsharp
{
    Write-Host
    Hdr('BuildCsharp')

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $resourcesPath = Join-Path $cssearchPath 'CsSearchLib' 'Resources'
    $testResourcesPath = Join-Path $cssearchPath 'CsSearchTests' 'Resources'

    # copy the shared json files to the local resource location
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the shared test files to the local test resource location
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $cssearchPath

    $csSearchSolutionPath = Join-Path $cssearchPath 'CsSearch.sln'

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
    }

    if ($release)
    {
        # add release to bin
        $cssearchExe = Join-Path $cssearchPath 'bin' 'cssearch.release.ps1'
        AddToBin($cssearchExe)
    }
    else
    {
        # add debug to bin
        $cssearchExe = Join-Path $cssearchPath 'bin' 'cssearch.debug.ps1'
        AddToBin($cssearchExe)
    }

    Set-Location $oldPwd
}

function BuildDart
{
    Write-Host
    Hdr('BuildDart')

    # ensure dart is installed
    if (-not (Get-Command 'dart' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dart')
        return
    }

    $oldPwd = Get-Location
    Set-Location $dartsearchPath

    Log('Building dartsearch')
    if ((-not (Test-Path (Join-Path $dartsearchPath '.dart_tool' 'package_config.json'))) -and
        (-not (Test-Path (Join-Path $dartsearchPath '.packages'))))
    {
        Log('dart pub get')
        dart pub get
    }
    else
    {
        Log('dart pub upgrade')
        dart pub upgrade
    }

    # add to bin
    $dartsearchExe = Join-Path $dartsearchPath 'bin' 'dartsearch.ps1'
    AddToBin($dartsearchExe)

    Set-Location $oldPwd
}

function BuildFsharp
{
    Write-Host
    Hdr('BuildFsharp')

    # ensure dotnet is installed
    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        return
    }

    $resourcesPath = Join-Path $fssearchPath 'FsSearchLib' 'Resources'
    $testResourcesPath = Join-Path $fssearchPath 'FsSearchTests' 'Resources'

    # copy the shared json files to the local resource location
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the shared test files to the local test resource location
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $fssearchPath

    $fsSearchSolutionPath = Join-Path $fssearchPath 'FsSearch.sln'

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
    }

    if ($release)
    {
        # add release to bin
        $fssearchExe = Join-Path $fssearchPath 'bin' 'fssearch.release.ps1'
        AddToBin($fssearchExe)
    }
    else
    {
        # add debug to bin
        $fssearchExe = Join-Path $fssearchPath 'bin' 'fssearch.debug.ps1'
        AddToBin($fssearchExe)
    }

    Set-Location $oldPwd
}

function BuildGo
{
    Write-Host
    Hdr('BuildGo')

    # ensure go is installed
    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        return
    }

    $oldPwd = Get-Location
    Set-Location $gosearchPath

    # go fmt the gosearch source (for auto-generated code)
    Log('Auto-formatting gosearch')
    Log('go fmt ./...')
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if (-not (Test-Path $binPath))
    {
        New-Item -ItemType directory -Path $binPath
    }

    # if GOBIN not defined, set to BIN_PATH
    if (-not (Test-Path Env:GOBIN))
    {
        $env:GOBIN = $binPath
    }

    # now build gosearch
    Log('Building gosearch')
    Log('go install ./...')
    go install ./...

    if ($env:GOBIN -ne $binPath)
    {
        # add to bin
        $gosearchExe = Join-Path $env:GOBIN 'gosearch'
        AddToBin($gosearchExe)
    }

    Set-Location $oldPwd
}

function BuildHaskell
{
    Write-Host
    Hdr('BuildHaskell')

    # ensure stack is installed
    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        return
    }

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
    $resourcesPath = Join-Path $hssearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $hssearchPath

    # build with stack (via make)
    Log('Building hssearch')
    Log('stack setup')
    make setup

    Log('stack build')
    make build

    Log("stack install --local-bin-path $binPath")
    stack install --local-bin-path $binPath

    Set-Location $oldPwd
}

function BuildJava
{
    Write-Host
    Hdr('BuildJava')

    # ensure mvn is installed
    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install maven')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $javasearchPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $javasearchPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    # run maven clean package (skip testing as this is run via unittest.sh)
    Log('Building javasearch')
    Log("mvn -f $javasearchPath/pom.xml clean package -Dmaven.test.skip=true")
    mvn -f $javasearchPath/pom.xml clean package '-Dmaven.test.skip=true'

    # add to bin
    $javasearchExe = Join-Path $javasearchPath 'bin' 'javasearch.ps1'
    AddToBin($javasearchExe)
}

function BuildJavaScript
{
    Write-Host
    Hdr('BuildJavaScript')

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $jssearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $jssearchPath

    # run npm install and build
    Log('Building jssearch')
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    # add to bin
    $jssearchExe = Join-Path $jssearchPath 'bin' 'jssearch.ps1'
    AddToBin($jssearchExe)

    Set-Location $oldPwd
}

function BuildKotlin
{
    Write-Host
    Hdr('BuildKotlin')

    # ensure gradle is installed
    if (-not (Get-Command 'gradle' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install gradle')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $ktsearchPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $ktsearchPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $ktsearchPath

    # run a gradle build
    Log('Building ktsearch')
    Log('gradle --warning-mode all clean jar')
    gradle --warning-mode all clean jar

    # add to bin
    $ktsearchExe = Join-Path $ktsearchPath 'bin' 'ktsearch.ps1'
    AddToBin($ktsearchExe)

    Set-Location $oldPwd
}

function BuildObjc
{
    Write-Host
    Hdr('BuildObjc')

    $target = 'alltargets'

    # ensure xcode is installed
    if (-not (Get-Command 'xcodebuild' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install Xcode')
        return
    }

    $oldPwd = Get-Location
    Set-Location $objcsearchPath

    $configurations = @()
    if ($debug)
    {
        $configurations += 'Debug'
    }
    if ($release)
    {
        $configurations += 'Release'
    }

    # run build for selected configurations
    ForEach ($c in $configurations)
    {
        if ($target -eq 'alltargets')
        {
            Log("xcodebuild -alltargets -configuration $c")
            xcodebuild -alltargets -configuration $c
        }
        else
        {
            Log("xcodebuild -project $target -configuration $c")
            xcodebuild -project $target -configuration $c
        }
    }

    if ($release)
    {
        # add release to bin
        $objcsearchExe = Join-Path $objcsearchPath 'bin' 'objcsearch.release.ps1'
        AddToBin($objcsearchExe)
    }
    else
    {
        # add debug to bin
        $objcsearchExe = Join-Path $objcsearchPath 'bin' 'objcsearch.debug.ps1'
        AddToBin($objcsearchExe)
    }

    Set-Location $oldPwd
}

function BuildOcaml
{
    Write-Host
    Hdr('BuildOcaml - currently unimplemented')
}

function BuildPerl
{
    Write-Host
    Hdr('BuildPerl')

    # ensure perl is installed
    if (-not (Get-Command 'perl' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install perl')
        return
    }

    $versionOutput = & perl -v | Select-String -Pattern 'This is perl 5' 2>&1
    if (-not $versionOutput)
    {
        PrintError('A 5.x version of perl is required')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $plsearchPath 'share'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # add to bin
    $plsearchExe = Join-Path $plsearchPath 'bin' 'plsearch.ps1'
    AddToBin($plsearchExe)
}

function BuildPhp
{
    Write-Host
    Hdr('BuildPhp')

    # ensure php is installed
    if (-not (Get-Command 'php' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install php')
        return
    }

    $versionOutput = & php -v | Select-String -Pattern 'PHP [78]' 2>&1
    if (-not $versionOutput)
    {
        PrintError('A version of PHP >= 7.x is required')
        return
    }

    # ensure composer is installed
    if (-not (Get-Command 'composer' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install composer')
        return
    }

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $sharedPath 'config.json'
    $configPath = Join-Path $phpsearchPath 'config'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $phpsearchPath 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $phpsearchPath

    # run a composer build
    Log('Building phpsearch')

    if (Test-Path (Join-Path $phpsearchPath 'vendor'))
    {
        Log('composer update')
        composer update
    }
    else
    {
        Log('composer install')
        composer install
    }

    # add to bin
    $phpsearchExe = Join-Path $phpsearchPath 'bin' 'phpsearch.ps1'
    AddToBin($phpsearchExe)

    Set-Location $oldPwd
}

function BuildPython
{
    Write-Host
    Hdr('BuildPython')

    # ensure python3.7+ is installed
    $pythonVersions = @('python3.11', 'python3.10', 'python3.9', 'python3.8', 'python3.7')
    $python = ''
    ForEach ($p in $pythonVersions)
    {
        if (Get-Command $p -ErrorAction 'SilentlyContinue')
        {
            $python = $p
            Log("Using $python")
            break
        }
    }

    if (-not $python)
    {
        PrintError('You need to install python(>= 3.7)')
        return
    }

    # Set to $true to use venv
    $useVenv=$venv

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $pysearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $pysearchPath

    if ($useVenv)
    {
        # create a virtual env to run from and install to
        $venvPath = Join-Path $pysearchPath 'venv'
        if (-not (Test-Path $venvPath))
        {
            Log("$python -m venv venv")
            & $python -m venv venv
        }
    
        # activate the virtual env
        $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
        Log("$activatePath")
        & $activatePath
    }

    # install dependencies in requirements.txt
    Log('pip3 install -r requirements.txt')
    pip3 install -r requirements.txt

    if ($useVenv)
    {
        # deactivate at end of setup process
        Log('deactivate')
        deactivate
    }

    # add to bin
    $pysearchExe = Join-Path $pysearchPath 'bin' 'pysearch.ps1'
    AddToBin($pysearchExe)

    Set-Location $oldPwd
}

function BuildRuby
{
    Write-Host
    Hdr('BuildRuby')

    # ensure ruby2.x is installed
    if (-not (Get-Command 'ruby' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install ruby')
        return
    }

    $versionOutput = & ruby -v | Select-String -Pattern 'ruby 2' 2>&1
    if (-not $versionOutput)
    {
        PrintError('A version of ruby >= 2.x is required')
        return
    }

    # copy the shared config json file to the local config location
    $configFilePath = Join-Path $sharedPath 'config.json'
    $configPath = Join-Path $rbsearchPath 'data'
    if (-not (Test-Path $configPath))
    {
        New-Item -ItemType directory -Path $configPath
    }
    Log("Copy-Item $configFilePath -Destination $configPath")
    Copy-Item $configFilePath -Destination $configPath

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $rbsearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # add to bin
    $rbsearchExe = Join-Path $rbsearchPath 'bin' 'rbsearch.ps1'
    AddToBin($rbsearchExe)
}

function BuildRust
{
    Write-Host
    Hdr('BuildRust')

    # ensure cargo/rust is installed
    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rust')
    }

    $oldPwd = Get-Location
    Set-Location $rssearchPath

    Log('Building rssearch')

    if ($debug)
    {
        Log('cargo build')
        cargo build
    }

    if ($release)
    {
        Log('cargo build --release')
        cargo build --release

        # add release to bin
        $rssearchExe = Join-Path $rssearchPath 'bin' 'rssearch.release.ps1'
        AddToBin($rssearchExe)
    }
    else
    {
        # add debug to bin
        $rssearchExe = Join-Path $rssearchPath 'bin' 'rssearch.debug.ps1'
        AddToBin($rssearchExe)
    }

    Set-Location $oldPwd
}

function BuildScala
{
    Write-Host
    Hdr('BuildScala')

    # ensure sbt is installed
    if (-not (Get-Command 'sbt' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install scala + sbt')
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $scalasearchPath 'src' 'main' 'resources'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    # copy the test files to the local test resource location
    $testResourcesPath = Join-Path $scalasearchPath 'src' 'test' 'resources'
    if (-not (Test-Path $testResourcesPath))
    {
        New-Item -ItemType directory -Path $testResourcesPath
    }
    CopyTestResources($testResourcesPath)

    $oldPwd = Get-Location
    Set-Location $scalasearchPath

    # run sbt assembly
    Log('Building scalasearch')
    Log("sbt 'set test in assembly := {}' clean assembly")
    sbt 'set test in assembly := {}' clean assembly

    # add to bin
    $scalasearchExe = Join-Path $scalasearchPath 'bin' 'scalasearch.ps1'
    AddToBin($scalasearchExe)

    Set-Location $oldPwd
}

function BuildSwift
{
    Write-Host
    Hdr('BuildSwift')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        return
    }

    $oldPwd = Get-Location
    Set-Location $swiftsearchPath

    Log('Building swiftsearch')

    if ($debug)
    {
        Log('swift build')
        swift build
    }

    if ($release)
    {
        Log('swift build --configuration release')
        swift build --configuration release

        # add release to bin
        $swiftsearchExe = Join-Path $swiftsearchPath 'bin' 'swiftsearch.release.ps1'
        AddToBin($swiftsearchExe)
        }
    else
    {
        # add debug to bin
        $swiftsearchExe = Join-Path $swiftsearchPath 'bin' 'swiftsearch.debug.ps1'
        AddToBin($swiftsearchExe)
        }

    Set-Location $oldPwd
}

function BuildTypeScript
{
    Write-Host
    Hdr('BuildTypeScript')

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install node.js/npm')
        return
    }

    # copy the shared json files to the local resource location
    $resourcesPath = Join-Path $tssearchPath 'data'
    if (-not (Test-Path $resourcesPath))
    {
        New-Item -ItemType directory -Path $resourcesPath
    }
    CopyJsonResources($resourcesPath)

    $oldPwd = Get-Location
    Set-Location $tssearchPath

    # run npm install and build
    Log('Building tssearch')
    Log('npm install')
    npm install

    Log('npm run build')
    npm run build

    # add to bin
    $tssearchExe = Join-Path $tssearchPath 'bin' 'tssearch.ps1'
    AddToBin($tssearchExe)

    Set-Location $oldPwd
}

function BuildLinux
{
    Write-Host
    Hdr('BuildLinux')

    # Measure-Command { BuildC }

    # Measure-Command { BuildClojure }

    # Measure-Command { BuildCpp }

    Measure-Command { BuildCsharp }

    Measure-Command { BuildDart }

    Measure-Command { BuildFsharp }

    Measure-Command { BuildGo }

    Measure-Command { BuildJava }

    Measure-Command { BuildJavaScript }

    # Measure-Command { BuildKotlin }

    Measure-Command { BuildPerl }

    Measure-Command { BuildPhp }

    # Measure-Command { BuildPowerShell }

    Measure-Command { BuildPython }

    Measure-Command { BuildRuby }

    Measure-Command { BuildRust }

    # Measure-Command { BuildScala }

    Measure-Command { BuildSwift }

    Measure-Command { BuildTypeScript }
}

function BuildAll
{
    Write-Host
    Hdr('BuildAll')

    # Measure-Command { BuildC }

    Measure-Command { BuildClojure }

    Measure-Command { BuildCpp }

    Measure-Command { BuildCsharp }

    Measure-Command { BuildDart }

    Measure-Command { BuildFsharp }

    Measure-Command { BuildGo }

    Measure-Command { BuildHaskell }

    Measure-Command { BuildJava }

    Measure-Command { BuildJavaScript }

    Measure-Command { BuildKotlin }

    Measure-Command { BuildObjc }

    Measure-Command { BuildPerl }

    Measure-Command { BuildPhp }

    Measure-Command { BuildPython }

    Measure-Command { BuildRuby }

    Measure-Command { BuildRust }

    Measure-Command { BuildScala }

    Measure-Command { BuildSwift }

    Measure-Command { BuildTypeScript }
}

################################################################################
# Main function
################################################################################

function BuildMain
{
    param($lang='')

    switch ($lang)
    {
        'all'        { BuildAll }
        'linux'      { BuildLinux }
        # 'c'          { Measure-Command { BuildC } }
        'clj'        { Measure-Command { BuildClojure } }
        'clojure'    { Measure-Command { BuildClojure } }
        'cpp'        { Measure-Command { BuildCpp } }
        'cs'         { Measure-Command { BuildCsharp } }
        'csharp'     { Measure-Command { BuildCsharp } }
        'dart'       { Measure-Command { BuildDart } }
        'fs'         { Measure-Command { BuildFsharp } }
        'fsharp'     { Measure-Command { BuildFsharp } }
        'go'         { Measure-Command { BuildGo } }
        'haskell'    { Measure-Command { BuildHaskell } }
        'hs'         { Measure-Command { BuildHaskell } }
        'java'       { Measure-Command { BuildJava } }
        'javascript' { Measure-Command { BuildJavaScript } }
        'js'         { Measure-Command { BuildJavaScript } }
        'kotlin'     { Measure-Command { BuildKotlin } }
        'kt'         { Measure-Command { BuildKotlin } }
        'objc'       { Measure-Command { BuildObjc } }
        # 'ocaml'      { Measure-Command { BuildOcaml } }
        # 'ml'         { Measure-Command { BuildOcaml } }
        'perl'       { Measure-Command { BuildPerl } }
        'pl'         { Measure-Command { BuildPerl } }
        'php'        { Measure-Command { BuildPhp } }
        # 'powershell' { Measure-Command { BuildPowerShell } }
        # 'ps1'        { Measure-Command { BuildPowerShell } }
        'py'         { Measure-Command { BuildPython } }
        'python'     { Measure-Command { BuildPython } }
        'rb'         { Measure-Command { BuildRuby } }
        'ruby'       { Measure-Command { BuildRuby } }
        'rs'         { Measure-Command { BuildRust } }
        'rust'       { Measure-Command { BuildRust } }
        'scala'      { Measure-Command { BuildScala } }
        'swift'      { Measure-Command { BuildSwift } }
        'ts'         { Measure-Command { BuildTypeScript } }
        'typescript' { Measure-Command { BuildTypeScript } }
        default      { ExitWithError("Unknown option: $lang") }
    }
}

if ($help -or $lang -eq '')
{
    Usage
}

BuildMain $lang
