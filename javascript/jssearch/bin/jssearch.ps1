#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$jsSearchExe = Join-Path $env:XSEARCH_PATH 'javascript' 'jssearch' 'dist' 'jssearch.js'

if (Test-Path $jsSearchExe -PathType Leaf)
{
    & node $jsSearchExe $Args
}
else
{
    Write-Host 'jssearch executable not found, need to run build first'
}
