#!/usr/bin/env pwsh

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

$scalaVersion = '3.7.4'
$scalaSearchJarPath = Join-Path $env:XSEARCH_PATH 'scala' 'scalasearch' 'target' "scala-$scalaVersion"
$scalaSearchVersion = '0.1.0'
$scalaSearchJarName = "scalasearch-assembly-$scalaSearchVersion.jar"
$scalaSearchJar = Join-Path $scalaSearchJarPath $scalaSearchJarName

if (Test-Path $scalaSearchJar)
{
    & java -cp $scalaSearchJar 'scalasearch.SearchMain' $Args
}
else
{
    Write-Host 'scalasearch executable not found, need to run build first'
}
