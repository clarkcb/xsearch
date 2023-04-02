#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$javaSearchJarPath = Join-Path $env:XSEARCH_PATH 'java' 'javasearch' 'target'

$javaSearchJars = @(Get-ChildItem $javaSearchJarPath) |
    Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.jar' -and $_ -match 'javasearch' }

if ($javaSearchJars.count -gt 0)
{
    & java -cp $javaSearchJars[0] 'javasearch.SearchMain' $Args
}
else
{
    Write-Host 'javasearch executable not found, need to run build first'
}
