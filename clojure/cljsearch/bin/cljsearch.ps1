#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$cljSearchJarPath = Join-Path $env:XSEARCH_PATH 'clojure' 'cljsearch' 'target' 'uberjar'

$cljSearchJars = @(Get-ChildItem $cljSearchJarPath) |
    Where-Object{ !$_.PSIsContainer -and $_.Extension -eq '.jar' -and $_ -match 'cljsearch.+standalone' }

if ($cljSearchJars.count -gt 0)
{
    & java -jar $cljSearchJars[0] $Args
}
else
{
    Write-Host 'cljsearch executable not found, need to run build first'
}
