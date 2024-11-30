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

Write-Host "help: $help"
Write-Host "all: $all"
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
    Write-Host "`nUsage: unittest.ps1 [-help] {""all"" | lang [lang...]}`n"
    exit
}

function PrintFailedBuilds
{
    if ($global:failedBuilds.Length -gt 0)
    {
        $fbString = $global:failedBuilds -join ' '
        PrintError("Failed builds: $fbString")
    }
    else
    {
        Log("All builds succeeded")
    }
}


################################################################################
# Unit Test functions
################################################################################

function UnitTestBashSearch
{
    Write-Host
    Hdr('UnitTestBashSearch')
    Log('not implemented at this time')
}

function UnitTestCSearch
{
    Write-Host
    Hdr('UnitTestCSearch')

    $oldPwd = Get-Location
    Set-Location $cSearchPath

    Log('Unit-testing csearch')
    Log('make run_tests')
    make run_tests

    Set-Location $oldPwd
}

function UnitTestCljSearch
{
    Write-Host
    Hdr('UnitTestCljSearch')

    if (Get-Command 'clj' -ErrorAction 'SilentlyContinue')
    {
        # clj -version output looks like this: Clojure CLI version 1.11.4.1474
        $clojureVersion = clj -version 2>&1
        Log("clojure version: $clojureVersion")
    }

    if (-not (Get-Command 'lein' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install leiningen')
        $global:failedBuilds += 'cljfind'
        return
    }

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    $leinVersion = lein version
    Log("lein version: $leinVersion")

    $oldPwd = Get-Location
    Set-Location $cljSearchPath

    # Test with lein
    Log('Unit-testing cljsearch')
    Log('lein test')
    lein test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'cljfind'
    }

    Set-Location $oldPwd
}

function UnitTestCppSearch
{
    Write-Host
    Hdr('UnitTestCppSearch')

    # if cmake is installed, display version
    if (Get-Command 'cmake' -ErrorAction 'SilentlyContinue')
    {
        # cmake --version output looks like this: cmake version 3.30.2
        $cmakeVersion = cmake --version | Select-String -Pattern '^cmake version'
        $cmakeVersion = @($cmakeVersion -split '\s+')[2]
        Log("cmake version: $cmakeVersion")
    }

    $configurations = @('debug', 'release')
    ForEach ($c in $configurations)
    {
        $cmakeBuildDir = Join-Path $cppSearchPath "cmake-build-$c"

        if (Test-Path $cmakeBuildDir)
        {
            $cppSearchTestExe = Join-Path $cmakeBuildDir 'cppsearch-tests'
            if (Test-Path $cppSearchTestExe)
            {
                # run tests
                Log($cppSearchTestExe)
                & $cppSearchTestExe

                # check for success/failure
                if ($LASTEXITCODE -eq 0)
                {
                    Log('Tests succeeded')
                }
                else
                {
                    PrintError('Tests failed')
                    $global:failedBuilds += 'cppsearch'
                    return
                }
            }
            else
            {
                LogError("cppsearch-tests not found: $cppSearchTestExe")
                $global:failedBuilds += 'cppsearch'
            }
        }
        else
        {
            LogError("cmake build directory not found: $cmmakeBuildDir")
            $global:failedBuilds += 'cppsearch'
        }
    }
}

function UnitTestCsSearch
{
    Write-Host
    Hdr('UnitTestCsSearch')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'cssearch'
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $csSearchSolutionPath = Join-Path $csSearchPath 'CsSearch.sln'
    # $verbosity = 'quiet'
    $verbosity = 'minimal'
    # $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Log('Unit-testing cssearch')
    Write-Host "dotnet test $csSearchSolutionPath --verbosity $verbosity"
    dotnet test $csSearchSolutionPath --verbosity $verbosity

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'cssearch'
    }
}

function UnitTestDartSearch
{
    Write-Host
    Hdr('UnitTestDartSearch')

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

    # run tests
    Log('Unit-testing dartsearch')
    Log('dart run test')
    dart run test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'dartsearch'
    }

    Set-Location $oldPwd
}

function UnitTestExSearch
{
    Write-Host
    Hdr('UnitTestExSearch')

    if (Get-Command 'elixir' -ErrorAction 'SilentlyContinue')
    {
        $elixirVersion = elixir --version | Select-String -Pattern 'Elixir'
        Log("elixir version: $elixirVersion")
    }

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

    # run tests
    Log('Unit-testing exsearch')
    Log('mix test')
    mix test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'exsearch'
    }

    Set-Location $oldPwd
}

function UnitTestFsSearch
{
    Write-Host
    Hdr('UnitTestFsSearch')

    if (-not (Get-Command 'dotnet' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install dotnet')
        $global:failedBuilds += 'fssearch'
        return
    }

    $dotnetVersion = dotnet --version
    Log("dotnet version: $dotnetVersion")

    $fsSearchSolutionPath = Join-Path $fsSearchPath 'FsSearch.sln'
    # $verbosity = 'quiet'
    $verbosity = 'minimal'
    # $verbosity = 'normal'
    # $verbosity = 'detailed'

    # run tests
    Log('Unit-testing fssearch')
    Write-Host "dotnet test $fsSearchSolutionPath --verbosity $verbosity"
    dotnet test $fsSearchSolutionPath --verbosity $verbosity

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'fssearch'
    }
}

function UnitTestGoSearch
{
    Write-Host
    Hdr('UnitTestGoSearch')

    if (-not (Get-Command 'go' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install go')
        $global:failedBuilds += 'gosearch'
        return
    }

    $goVersion = (go version) -replace 'go version ',''
    Log("go version: $goVersion")

    $oldPwd = Get-Location
    Set-Location $goSearchPath

    # run tests
    Log('Unit-testing gosearch')
    Log('go test --cover ./...')
    go test --cover ./...

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'gosearch'
    }

    Set-Location $oldPwd
}

function UnitTestHsSearch
{
    Write-Host
    Hdr('UnitTestHsSearch')

    # if ghc is installed, display version
    if (Get-Command 'ghc' -ErrorAction 'SilentlyContinue')
    {
        $ghcVersion = ghc --version
        Log("ghc version: $ghcVersion")
    }

    if (-not (Get-Command 'stack' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install stack')
        $global:failedBuilds += 'hssearch'
        return
    }

    $stackVersion = stack --version
    Log("stack version: $stackVersion")

    $oldPwd = Get-Location
    Set-Location $hsSearchPath

    # test with stack
    Log('Unit-testing hssearch')
    Log('stack test')
    stack test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'hssearch'
    }

    Set-Location $oldPwd
}

function UnitTestJavaSearch
{
    Write-Host
    Hdr('UnitTestJavaSearch')

    # if java is installed, display version
    if (Get-Command 'java' -ErrorAction 'SilentlyContinue')
    {
        $javaVersion = java -version 2>&1 | Select-String -Pattern 'java version'
        Log("java version: $javaVersion")
    }

    if (-not (Get-Command 'mvn' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install mvn')
        $global:failedBuilds += 'javasearch'
        return
    }

    $mvnVersion = mvn --version 2>&1 | Select-String -Pattern 'Apache Maven'
    Log("mvn version: $mvnVersion")

    # run tests via maven
    Log('Unit-testing javasearch')
    $pomPath = Join-Path $javaSearchPath 'pom.xml'
    Log("mvn -f $pomPath test")
    mvn -f $pomPath test
}

function UnitTestJsSearch
{
    Write-Host
    Hdr('UnitTestJsSearch')

    # if node is installed, display version
    if (Get-Command 'node' -ErrorAction 'SilentlyContinue')
    {
        $nodeVersion = node --version
        Log("node version: $nodeVersion")
    }

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        $global:failedBuilds += 'jssearch'
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $oldPwd = Get-Location
    Set-Location $jsSearchPath

    # run tests via npm
    Log('Unit-testing jssearch')
    Log('npm test')
    npm test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'jssearch'
    }

    Set-Location $oldPwd
}

function UnitTestKtSearch
{
    Write-Host
    Hdr('UnitTestKtSearch')

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

    $oldPwd = Get-Location
    Set-Location $ktSearchPath

    # run tests via gradle
    Log('Unit-testing ktsearch')
    Log("$gradle --warning-mode all test")
    & $gradle --warning-mode all test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'ktsearch'
    }

    Set-Location $oldPwd
}

function UnitTestObjcSearch
{
    Write-Host
    Hdr('UnitTestObjcSearch')

    # ensure swift is installed
    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'objcsearch'
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $objcSearchPath

    Log('Unit-testing objcsearch')
    Log('swift test')
    swift test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'objcsearch'
    }

    Set-Location $oldPwd
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
        return
    }

    Log("perl version: $perlVersion")

    $plTestsPath = Join-Path $plSearchPath 't'

    Log('Unit-testing plsearch')
    $plTests = @(Get-ChildItem $plTestsPath |
        Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.pl' })
    ForEach ($plTest in $plTests)
    {
        Log("perl $plTest")
        perl $plTest
    }
}

function UnitTestPhpSearch
{
    Write-Host
    Hdr('UnitTestPhpSearch')

    # if php is installed, display version
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

    # if composer is installed, display version
    if (Get-Command 'composer' -ErrorAction 'SilentlyContinue')
    {
        $composerVersion = composer --version 2>&1 | Select-String -Pattern '^Composer'
        Log("composer version: $composerVersion")
    }

    if (-not (Get-Command 'phpunit' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install phpunit')
        $global:failedBuilds += 'phpsearch'
        return
    }

    $phpTestsPath = Join-Path $phpSearchPath 'tests'

    # run tests
    Log('Unit-testing phpsearch')
    Log("phpunit $phpTestsPath")
    phpunit $phpTestsPath

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'phpsearch'
    }
}

function UnitTestPs1Search
{
    Write-Host
    Hdr('UnitTestPs1Search')

    # We don't need to check for powershell, as we're running in it

    $powershellVersion = pwsh -v
    Log("powershell version: $powershellVersion")

    $testsScriptPath = Join-Path $ps1SearchPath 'ps1search.tests.ps1'
    if (-not (Test-Path $testsScriptPath))
    {
        Log("Test script not found: $testsScriptPath")
        $global:failedBuilds += 'ps1search'
        return
    }

    # run tests
    Log('Unit-testing ps1search')
    Log("& $testsScriptPath")
    & $testsScriptPath
}

function UnitTestPySearch
{
    Write-Host
    Hdr('UnitTestPySearch')

    $venvPath = Join-Path $pySearchPath 'venv'
    if (-not (Test-Path $venvPath))
    {
        PrintError('venv path not found, you probably need to run the python build (./build.ps1 python)')
        $global:failedBuilds += 'pyfind'
        return
    }

    $oldPwd = Get-Location
    Set-Location $pySearchPath

    # activate the virtual env
    $activatePath = Join-Path $venvPath 'bin' 'Activate.ps1'
    Log("$activatePath")
    & $activatePath

    Log('Unit-testing pysearch')
    # Run the individual tests
    Log('pytest')
    pytest

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'pysearch'
    }

    # deactivate at end of setup process
    Log('deactivate')
    deactivate

    Set-Location $oldPwd
}

function UnitTestRbSearch
{
    Write-Host
    Hdr('UnitTestRbSearch')

    # ensure ruby3.x is installed
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

    # ensure bundler is installed
    # if (-not (Get-Command 'bundle' -ErrorAction 'SilentlyContinue'))
    # {
    #     PrintError('You need to install bundler: https://bundler.io/')
    #     return
    # }

    # ensure rake is installed
    if (-not (Get-Command 'rake' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install rake')
        $global:failedBuilds += 'rbsearch'
        return
    }

    $oldPwd = Get-Location
    Set-Location $rbSearchPath

    Log('Unit-testing rbsearch')
    Log('bundle exec rake test')
    bundle exec rake test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'rbsearch'
    }

    Set-Location $oldPwd
}

function UnitTestRsSearch
{
    Write-Host
    Hdr('UnitTestRsSearch')

    # if rust is installed, display version
    if (-not (Get-Command 'rustc' -ErrorAction 'SilentlyContinue'))
    {
        $rustVersion = rustc --version | Select-String -Pattern 'rustc'
        Log("rustc version: $rustVersion")
    }

    if (-not (Get-Command 'cargo' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install cargo')
        $global:failedBuilds += 'rssearch'
        return
    }

    $cargoVersion = cargo --version
    Log("cargo version: $cargoVersion")

    $oldPwd = Get-Location
    Set-Location $rsSearchPath

    # run tests
    Log('Unit-testing rssearch')
    Log('cargo test --package rssearch --bin rssearch')
    cargo test --package rssearch --bin rssearch

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'rssearch'
    }

    Set-Location $oldPwd
}

function UnitTestScalaSearch
{
    Write-Host
    Hdr('UnitTestScalaSearch')

    # if scala is installed, display version
    if (Get-Command 'scala' -ErrorAction 'SilentlyContinue')
    {
        $scalaVersion = scala --version 2>&1 | Select-Object -Last 1
        Log($scalaVersion)
    }

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

    $oldPwd = Get-Location
    Set-Location $scalaSearchPath

    # run tests
    Log('Unit-testing scalasearch')
    Log('sbt test')
    sbt test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'scalasearch'
    }

    Set-Location $oldPwd
}

function UnitTestSwiftSearch
{
    Write-Host
    Hdr('UnitTestSwiftSearch')

    if (-not (Get-Command 'swift' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install swift')
        $global:failedBuilds += 'swiftsearch'
        return
    }

    $swiftVersion = swift --version 2>&1 | Select-String -Pattern 'Swift'
    Log("swift version: $swiftVersion")

    $oldPwd = Get-Location
    Set-Location $swiftSearchPath

    # run tests
    Log('Unit-testing swiftsearch')
    Log('swift test')
    swift test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'swiftsearch'
    }

    Set-Location $oldPwd
}

function UnitTestTsSearch
{
    Write-Host
    Hdr('UnitTestTsSearch')

    # if node is installed, display version
    if (Get-Command 'node' -ErrorAction 'SilentlyContinue')
    {
        $nodeVersion = node --version
        Log("node version: $nodeVersion")
    }

    # ensure npm is installed
    if (-not (Get-Command 'npm' -ErrorAction 'SilentlyContinue'))
    {
        PrintError('You need to install npm')
        $global:failedBuilds += 'tssearch'
        return
    }

    $npmVersion = npm --version
    Log("npm version: $npmVersion")

    $oldPwd = Get-Location
    Set-Location $tsSearchPath

    # run tests
    Log('Unit-testing tssearch')
    Log('npm test')
    npm test

    # check for success/failure
    if ($LASTEXITCODE -eq 0)
    {
        Log('Tests succeeded')
    }
    else
    {
        PrintError('Tests failed')
        $global:failedBuilds += 'tssearch'
    }

    Set-Location $oldPwd
}

function UnitTestAll
{
    Write-Host
    Hdr('UnitTestAll')

    # UnitTestBashSearch

    # UnitTestCSearch

    UnitTestCljSearch

    UnitTestCppSearch

    UnitTestCsSearch

    UnitTestDartSearch

    UnitTestExSearch

    UnitTestFsSearch

    UnitTestGoSearch

    # UnitTestGroovySearch

    UnitTestHsSearch

    UnitTestJavaSearch

    UnitTestJsSearch

    UnitTestKtSearch

    UnitTestObjcSearch

    UnitTestMlSearch

    UnitTestPlSearch

    UnitTestPhpSearch

    UnitTestPs1Search

    UnitTestPySearch

    UnitTestRbSearch

    UnitTestRsSearch

    UnitTestScalaSearch

    UnitTestSwiftSearch

    UnitTestTsSearch

    PrintFailedBuilds

    exit
}

################################################################################
# Main function
################################################################################

function UnitTestMain
{
    param($langs=@())

    if ($langs.Count -eq 0)
    {
        Usage
    }

    if ($langs -contains 'all')
    {
        UnitTestAll
    }

    ForEach ($lang in $langs)
    {
        switch ($lang)
        {
            'bash'       { UnitTestBashSearch }
            # 'c'          { UnitTestCSearch }
            'clj'        { UnitTestCljSearch }
            'clojure'    { UnitTestCljSearch }
            'cpp'        { UnitTestCppSearch }
            'cs'         { UnitTestCsSearch }
            'csharp'     { UnitTestCsSearch }
            'dart'       { UnitTestDartSearch }
            'elixir'     { UnitTestExSearch }
            'ex'         { UnitTestExSearch }
            'fs'         { UnitTestFsSearch }
            'fsharp'     { UnitTestFsSearch }
            'go'         { UnitTestGoSearch }
            # 'groovy'     { UnitTestGroovySearch }
            'haskell'    { UnitTestHsSearch }
            'hs'         { UnitTestHsSearch }
            'java'       { UnitTestJavaSearch }
            'javascript' { UnitTestJsSearch }
            'js'         { UnitTestJsSearch }
            'kotlin'     { UnitTestKtSearch }
            'kt'         { UnitTestKtSearch }
            'objc'       { UnitTestObjcSearch }
            'ocaml'      { UnitTestMlSearch }
            'ml'         { UnitTestMlSearch }
            'perl'       { UnitTestPlSearch }
            'pl'         { UnitTestPlSearch }
            'php'        { UnitTestPhpSearch }
            'powershell' { UnitTestPs1Search }
            'ps1'        { UnitTestPs1Search }
            'pwsh'       { UnitTestPs1Search }
            'py'         { UnitTestPySearch }
            'python'     { UnitTestPySearch }
            'rb'         { UnitTestRbSearch }
            'ruby'       { UnitTestRbSearch }
            'rs'         { UnitTestRsSearch }
            'rust'       { UnitTestRsSearch }
            'scala'      { UnitTestScalaSearch }
            'swift'      { UnitTestSwiftSearch }
            'ts'         { UnitTestTsSearch }
            'typescript' { UnitTestTsSearch }
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
        UnitTestAll
    }

    UnitTestMain $langs
}
catch {
    PrintError($_.Exception.Message)
}
finally {
    Set-Location $oldPwd
}
