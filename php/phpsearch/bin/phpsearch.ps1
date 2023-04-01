#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$phpSearchExe = Join-Path $env:XSEARCH_PATH 'php' 'phpsearch' 'bin' 'phpsearch.php'

if (Test-Path $phpSearchExe -PathType Leaf)
{
    & php $phpSearchExe $Args
}
else
{
    Write-Host 'phpsearch executable not found, need to run build first'
}
