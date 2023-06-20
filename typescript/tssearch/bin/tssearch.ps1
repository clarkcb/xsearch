#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$tsSearchExe = Join-Path $env:XSEARCH_PATH 'typescript' 'tssearch' 'dist' 'tssearch.js'

if (Test-Path $tsSearchExe -PathType Leaf)
{
    & node $tsSearchExe $Args
}
else
{
    Write-Host 'tssearch executable not found, need to run build first'
}
