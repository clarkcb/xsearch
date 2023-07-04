#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$objcSearchExe = Join-Path $env:XSEARCH_PATH 'objc' 'objcsearch' '.build' 'debug' 'objcsearchApp'

if (Test-Path $objcSearchExe -PathType Leaf)
{
    & $objcSearchExe $Args
}
else
{
    Write-Host 'objcsearch executable not found, need to run build first'
}
