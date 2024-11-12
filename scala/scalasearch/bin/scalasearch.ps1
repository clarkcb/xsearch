#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$scalaVersion = '3.5.2'
$scalaSearchJarPath = Join-Path $env:XSEARCH_PATH 'scala' 'scalasearch' 'target' "scala-$scalaVersion"

$scalaSearchJars = @(Get-ChildItem $scalaSearchJarPath) |
    Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.jar' -and $_ -match 'scalasearch-assembly' }

if ($scalaSearchJars.count -gt 0)
{
    & java -cp $scalaSearchJars[0] 'scalasearch.SearchMain' $Args
}
else
{
    Write-Host 'scalasearch executable not found, need to run build first'
}
