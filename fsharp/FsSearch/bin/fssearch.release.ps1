#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$fsSearchPath = Join-Path $env:XSEARCH_PATH 'fsharp' 'FsSearch'

# $configuration = 'Debug'
$configuration = 'Release'
$dotNetVersion = 'net9.0'
$fsSearchExe = Join-Path $fsSearchPath 'FsSearch' 'bin' $configuration $dotNetVersion 'FsSearch'

if (Test-Path $fsSearchExe -PathType Leaf)
{
    & $fsSearchExe $Args
}
else
{
    Write-Host 'fssearch executable not found, need to run build first'
}
