#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$rbSearchExe = Join-Path $env:XSEARCH_PATH 'ruby' 'rbsearch' 'bin' 'rbsearch.rb'

if (Test-Path $rbSearchExe -PathType Leaf)
{
    & ruby $rbSearchExe $Args
}
else
{
    Write-Host 'rbsearch executable not found, need to run build first'
}
