#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$swiftSearchExe = Join-Path $env:XSEARCH_PATH 'swift' 'swiftsearch' '.build' 'release' 'swiftsearchApp'

if (Test-Path $swiftSearchExe -PathType Leaf)
{
    & $swiftSearchExe $Args
}
else
{
    Write-Host 'swiftsearch executable not found, need to run build first'
}
