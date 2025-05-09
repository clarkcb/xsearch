#!/usr/bin/env pwsh

if (-not (Test-Path Env:XFIND_PATH))
{
    $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind'
}

$javaFindVersion = "0.1.0-SNAPSHOT"
$javaFindJarPath = Join-Path $env:XFIND_PATH 'java' 'javafind' 'build' 'libs' "javafind-$javaFindVersion.jar"

if (-not (Test-Path Env:XSEARCH_PATH))
{
    $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch'
}

# $javaSearchVersion = "0.1.0-SNAPSHOT"
$javaSearchLibJarPath = Join-Path $env:XSEARCH_PATH 'java' 'javasearch' 'lib' 'build' 'libs' 'lib.jar'
$javaSearchAppJarPath = Join-Path $env:XSEARCH_PATH 'java' 'javasearch' 'app' 'build' 'libs' 'app.jar'
$javaSearchJarsPath = Join-Path $env:XSEARCH_PATH 'java' 'javasearch' 'target'

$javaSearchClasspath = "$($javaFindJarPath):$($javaSearchLibJarPath):$javaSearchAppJarPath"

$oldJavaHome = $env:JAVA_HOME
# $env:JAVA_HOME = "/usr/local/Cellar/openjdk/23.0.1/libexec/openjdk.jdk/Contents/Home"
$env:JAVA_HOME = /usr/libexec/java_home -v17


java -cp $javaSearchClasspath 'javasearch.JavaSearch' $Args

$env:JAVA_HOME = $oldJavaHome
