#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$plSearchExe = Join-Path $env:XSEARCH_PATH 'perl' 'plsearch' 'bin' 'plsearch.pl'

if (Test-Path $plSearchExe -PathType Leaf)
{
    & perl $plSearchExe $Args
}
else
{
    Write-Host 'plsearch executable not found, need to run build first'
}
