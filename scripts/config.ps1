################################################################################
#
# config.ps1
#
# The common configuration for PowerShell scripts
#
################################################################################

########################################
# Configuration
########################################

# XSEARCH_PATH defaults to $HOME/src/xsearch if not defined
if (-not (Test-Path env:XSEARCH_PATH)) { $env:XSEARCH_PATH = Join-Path $env:HOME 'src' 'xsearch' }
$xsearchPath = $env:XSEARCH_PATH
$xsearchBinPath = Join-Path $xsearchPath 'bin'
$xsearchSharedPath = Join-Path $xsearchPath 'shared'
$xsearchTestFilePath = Join-Path $xsearchSharedPath 'testFiles'

# Language roots
# $cPath = Join-Path $xsearchPath 'c'
$xsearchClojurePath = Join-Path $xsearchPath 'clojure'
$xsearchCppPath = Join-Path $xsearchPath 'cpp'
$xsearchCsharpPath = Join-Path $xsearchPath 'csharp'
$xsearchDartPath = Join-Path $xsearchPath 'dart'
$xsearchElixirPath = Join-Path $xsearchPath 'elixir'
$xsearchFsharpPath = Join-Path $xsearchPath 'fsharp'
$xsearchGoPath = Join-Path $xsearchPath 'go'
# $xsearchGroovyPath = Join-Path $xsearchPath 'groovy'
$xsearchHaskellPath = Join-Path $xsearchPath 'haskell'
$xsearchJavaPath = Join-Path $xsearchPath 'java'
$xsearchJavascriptPath = Join-Path $xsearchPath 'javascript'
$xsearchKotlinPath = Join-Path $xsearchPath 'kotlin'
$xsearchObjcPath = Join-Path $xsearchPath 'objc'
# $xsearchOcamlPath = Join-Path $xsearchPath 'ocaml'
$xsearchPerlPath = Join-Path $xsearchPath 'perl'
$xsearchPhpPath = Join-Path $xsearchPath 'php'
# $xsearchPowershellPath = Join-Path $xsearchPath 'powershell'
$xsearchPythonPath = Join-Path $xsearchPath 'python'
$xsearchRubyPath = Join-Path $xsearchPath 'ruby'
$xsearchRustPath = Join-Path $xsearchPath 'rust'
$xsearchScalaPath = Join-Path $xsearchPath 'scala'
$xsearchSwiftPath = Join-Path $xsearchPath 'swift'
$xsearchTypescriptPath = Join-Path $xsearchPath 'typescript'

# Language version roots
# $csearchPath = Join-Path $cPath 'csearch'
$cljsearchPath = Join-Path $xsearchClojurePath 'cljsearch'
$cppsearchPath = Join-Path $xsearchCppPath 'cppsearch'
$cssearchPath = Join-Path $xsearchCsharpPath 'cssearch'
$dartsearchPath = Join-Path $xsearchDartPath 'dartsearch'
$exsearchPath = Join-Path $xsearchElixirPath 'exsearch'
$fssearchPath = Join-Path $xsearchFsharpPath 'fssearch'
$gosearchPath = Join-Path $xsearchGoPath 'gosearch'
# $groovysearchPath = Join-Path $xsearchGroovyPath 'groovysearch'
$hssearchPath = Join-Path $xsearchHaskellPath 'hssearch'
$javasearchPath = Join-Path $xsearchJavaPath 'javasearch'
$jssearchPath = Join-Path $xsearchJavascriptPath 'jssearch'
$ktsearchPath = Join-Path $xsearchKotlinPath 'ktsearch'
$objcsearchPath = Join-Path $xsearchObjcPath 'objcsearch'
# $mlsearchPath = Join-Path $xsearchOcamlPath 'mlsearch'
$phpsearchPath = Join-Path $xsearchPhpPath 'phpsearch'
$plsearchPath = Join-Path $xsearchPerlPath 'plsearch'
# $ps1searchPath = Join-Path $xsearchPowershellPath 'ps1search'
$pysearchPath = Join-Path $xsearchPythonPath 'pysearch'
$rbsearchPath = Join-Path $xsearchRubyPath 'rbsearch'
$rssearchPath = Join-Path $xsearchRustPath 'rssearch'
$scalasearchPath = Join-Path $xsearchScalaPath 'scalasearch'
$swiftsearchPath = Join-Path $xsearchSwiftPath 'swiftsearch'
$tssearchPath = Join-Path $xsearchTypescriptPath 'tssearch'
