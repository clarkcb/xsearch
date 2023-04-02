#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$pySearchExe = Join-Path $env:XSEARCH_PATH 'python' 'pysearch' 'bin' 'pysearch.py'

if (Test-Path $pySearchExe -PathType Leaf)
{
    & python3.11 $pySearchExe $Args
}
else
{
    Write-Host 'pysearch executable not found, need to run build first'
}
