################################################################################
#
# unittest.ps1
#
# Runs unit tests for specified language version of xsearch, or all versions
#
################################################################################
param([string]$lang="all")

########################################
# Configuration
########################################

. $PSScriptRoot\config.ps1
. $PSScriptRoot\common.ps1


################################################################################
# Unit Test functions
################################################################################

function UnitTestClojure
{
    Write-Host
    Log("UnitTestClojure - currently unsupported")
}

function UnitTestCsharp
{
    Write-Host
    Log("UnitTestCsharp")

    $csharpPath = Join-Path -Path $xsearchPath -ChildPath "csharp"
    $cssearchSolutionPath = Join-Path -Path $csharpPath -ChildPath "CsSearch"
    $cssearchTestsPath = Join-Path -Path $cssearchSolutionPath -ChildPath "CsSearchTests"
    $configuration = "Debug";
    #$configuration = "Release";

    Log("Unit-testing cssearch")
    Write-Host "$cssearchTestsPath\bin\$configuration\CsSearchTests.exe"
    &$cssearchTestsPath\bin\$configuration\CsSearchTests.exe
}

function UnitTestFsharp
{
    Write-Host
    Log("UnitTestFsharp - currently unsupported")
}

function UnitTestGo
{
    Write-Host
    Log("UnitTestGo")
    $goPath = Join-Path -Path $xsearchPath -ChildPath "go"
    $gosearchPath = "$goPath\src\elocale.com\clarkcb\xsearch"

    $oldPwd = Get-Location
    Set-Location $gosearchPath

    Log("Unit-testing gosearch")
    Write-Host "go test"
    go test

    Set-Location $oldPwd
}

function UnitTestHaskell
{
    Write-Host
    Log("UnitTestHaskell")
    $haskellPath = Join-Path -Path $xsearchPath -ChildPath "haskell"
    $hssearchPath = Join-Path -Path $haskellPath -ChildPath "hssearch"

    $oldPwd = Get-Location
    Set-Location $hssearchPath

    Log("Unit-testing hssearch")
    Write-Host "cabal test"
    cabal test

    Set-Location $oldPwd
}

function UnitTestJava
{
    Write-Host
    Log("UnitTestJava - currently unsupported")
}

function UnitTestJavaScript
{
    Write-Host
    Log("UnitTestJavaScript")
    $javascriptPath = Join-Path -Path $xsearchPath -ChildPath "javascript"
    $jssearchPath = Join-Path -Path $javascriptPath -ChildPath "jssearch"
    $testsPath = Join-Path -Path $jssearchPath -ChildPath "tests"
    $nodeUnitPath = "$jssearchPath\node_modules\nodeunit"
    $nodeUnit = "$javascriptUnitPath\bin\nodeunit"

    $oldPwd = Get-Location

    if (!(Test-Path $nodeUnitPath))
    {
        Log("nodeunit not installed, installing")
        Set-Location $jssearchPath
        Write-Host "npm install nodeunit"
        npm install nodeunit
    }

    Set-Location $testsPath

    Log("Unit-testing nodesearch")
    $tests = @(Get-ChildItem $testsPath |
        ?{ !$_.PSIsContainer -and $_.Extension -eq '.js' })
    ForEach ($test in $tests)
    {
        Write-Host "`nnodeunit $test"
        node $nodeUnit $test
    }

    Set-Location $oldPwd
}

function UnitTestPerl
{
    Write-Host
    Log("UnitTestPerl")
    $perlPath = Join-Path -Path $xsearchPath -ChildPath "perl"
    $plTestsPath = Join-Path -Path $perlPath -ChildPath "tests"

    $oldPwd = Get-Location
    Set-Location $plTestsPath

    Log("Unit-testing plsearch")
    $pltests = @(Get-ChildItem $plTestsPath |
        ?{ !$_.PSIsContainer -and $_.Extension -eq '.pl' })
    ForEach ($pltest in $pltests)
    {
        Write-Host "`nperl $pltest"
        perl $pltest
    }

    Set-Location $oldPwd
}

function UnitTestPhp
{
    Write-Host
    Log("UnitTestPhp - currently unsupported")
}

function UnitTestPython
{
    Write-Host
    Log("UnitTestPython")
    $pythonPath = Join-Path -Path $xsearchPath -ChildPath "python"
    $pyTestsPath = Join-Path -Path $pythonPath -ChildPath "tests"

    $oldPwd = Get-Location
    Set-Location $pyTestsPath

    Log("Unit-testing pysearch")
    $pytests = @(Get-ChildItem $pyTestsPath |
        ?{ !$_.PSIsContainer -and $_.Extension -eq '.py' })
    ForEach ($pytest in $pytests)
    {
        Write-Host "`npython $pytest"
        python $pytest
    }

    Set-Location $oldPwd
}

function UnitTestRuby
{
    Write-Host
    Log("UnitTestRuby")
    $rubyPath = Join-Path -Path $xsearchPath -ChildPath "ruby"
    $rbTestsPath = Join-Path -Path $rubyPath -ChildPath "tests"

    $oldPwd = Get-Location
    Set-Location $rbTestsPath

    Log("Unit-testing rbsearch")
    $rbtests = @(Get-ChildItem $rbTestsPath |
        ?{ !$_.PSIsContainer -and $_.Extension -eq '.rb' })
    ForEach ($rbtest in $rbtests)
    {
        Write-Host "`nruby $rbtest"
        ruby $rbtest
    }

    Set-Location $oldPwd
}

function UnitTestScala
{
    Write-Host
    Log("UnitTestScala - currently unsupported")
}

function UnitTestTypeScript
{
    Write-Host
    Log("UnitTestTypeScript - currently unsupported")
}

function UnitTestAll
{
    Write-Host
    Log("UnitTestAll")

    UnitTestClojure

    UnitTestCsharp

    UnitTestFsharp

    UnitTestGo

    UnitTestHaskell

    UnitTestJava

    UnitTestJavaScript

    UnitTestPerl

    UnitTestPhp

    UnitTestPython

    UnitTestRuby

    UnitTestScala

    UnitTestTypeScript
}

################################################################################
# Main function
################################################################################

function UnitTestMain
{
    param($lang="all")

    switch ($lang)
    {
        "all"        { UnitTestAll }
        "clojure"    { UnitTestClojure }
        "csharp"     { UnitTestCsharp }
        "fsharp"     { UnitTestFsharp }
        "go"         { UnitTestGo }
        "haskell"    { UnitTestHaskell }
        "java"       { UnitTestJava }
        "javascript" { UnitTestJavaScript }
        "perl"       { UnitTestPerl }
        "php"        { UnitTestPhp }
        "python"     { UnitTestPython }
        "ruby"       { UnitTestRuby }
        "scala"      { UnitTestScala }
        "typescript" { UnitTestTypeScript }
        default      { ExitWithError("Unknown option: $lang") }
    }
}

UnitTestMain $lang
