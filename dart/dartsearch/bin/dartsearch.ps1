#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$dartSearchPath = Join-Path $env:XSEARCH_PATH 'dart' 'dartsearch'

$packagesPath = Join-Path $dartSearchPath '.packages'
$dartSearchExe = Join-Path $dartSearchPath 'bin' 'dartsearch.dart'

if (Test-Path $packagesPath -PathType Leaf)
{
    & dart --packages="$packagesPath" $dartSearchExe $Args
}
else
{
    Write-Host 'dartsearch executable not found, need to run build first'
}
