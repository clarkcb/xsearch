#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$buildProfile = 'debug'
# $buildProfile = 'release'

$rsSearchExe = Join-Path $env:XSEARCH_PATH 'rust' 'rssearch' 'target' $buildProfile 'rssearch'

if (Test-Path $rsSearchExe -PathType Leaf)
{
    & $rsSearchExe $Args
}
else
{
    Write-Host 'rssearch executable not found, need to run build first'
}
