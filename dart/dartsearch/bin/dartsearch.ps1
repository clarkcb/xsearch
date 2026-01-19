#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$dartSearchPath = Join-Path $env:XSEARCH_PATH 'dart' 'dartsearch'
$dartSearchExe = Join-Path $dartSearchPath 'bin' 'dartsearch.exe'

if (Test-Path $dartSearchExe -PathType Leaf)
{
    & $dartSearchExe $Args
}
else
{
    Write-Host 'dartsearch executable not found, need to run build first'
}
