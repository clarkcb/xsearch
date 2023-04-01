#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$cppSearchPath = Join-Path $env:XSEARCH_PATH 'cpp' 'cppsearch'

$configuration = 'debug'
# $configuration = 'release'
$cmakeBuildPath = Join-Path $cppSearchPath "cmake-build-$configuration"
$cppSearchExe = Join-Path $cmakeBuildPath 'cppsearch'

if (Test-Path $cppSearchExe -PathType Leaf)
{
    & $cppSearchExe $Args
}
else
{
    Write-Host 'cppsearch executable not found, need to run build first'
}
