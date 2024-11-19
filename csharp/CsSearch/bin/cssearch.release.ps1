#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$csSearchPath = Join-Path $env:XSEARCH_PATH 'csharp' 'CsSearch'

# $configuration = 'Debug'
$configuration = 'Release'
$dotNetVersion = 'net9.0'
$csSearchExe = Join-Path $csSearchPath 'CsSearch' 'bin' $configuration $dotNetVersion 'CsSearch'

if (Test-Path $csSearchExe -PathType Leaf)
{
    & $csSearchExe $Args
}
else
{
    Write-Host 'cssearch executable not found, need to run build first'
}
