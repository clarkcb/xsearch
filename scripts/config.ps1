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
if (-not (Test-Path env:XSEARCH_PATH)) { $env:XSEARCH_PATH = Join-Path $HOME 'src' 'xsearch' }
$xsearchPath = $env:XSEARCH_PATH
$binPath = Join-Path $xsearchPath 'bin'
$sharedPath = Join-Path $xsearchPath 'shared'
$testFilePath = Join-Path $sharedPath 'testFiles'

# Language roots
# $cPath = Join-Path $xsearchPath 'c'
$clojurePath = Join-Path $xsearchPath 'clojure'
$cppPath = Join-Path $xsearchPath 'cpp'
$csharpPath = Join-Path $xsearchPath 'csharp'
$dartPath = Join-Path $xsearchPath 'dart'
$fsharpPath = Join-Path $xsearchPath 'fsharp'
$goPath = Join-Path $xsearchPath 'go'
$haskellPath = Join-Path $xsearchPath 'haskell'
$javaPath = Join-Path $xsearchPath 'java'
$javascriptPath = Join-Path $xsearchPath 'javascript'
$kotlinPath = Join-Path $xsearchPath 'kotlin'
$objcPath = Join-Path $xsearchPath 'objc'
# $ocamlPath = Join-Path $xsearchPath 'ocaml'
$perlPath = Join-Path $xsearchPath 'perl'
$phpPath = Join-Path $xsearchPath 'php'
# $powershellPath = Join-Path $xsearchPath 'powershell'
$pythonPath = Join-Path $xsearchPath 'python'
$rubyPath = Join-Path $xsearchPath 'ruby'
$rustPath = Join-Path $xsearchPath 'rust'
$scalaPath = Join-Path $xsearchPath 'scala'
$swiftPath = Join-Path $xsearchPath 'swift'
$typescriptPath = Join-Path $xsearchPath 'typescript'

# Language version roots
# $csearchPath = Join-Path $cPath 'csearch'
$cljsearchPath = Join-Path $clojurePath 'cljsearch'
$cppsearchPath = Join-Path $cppPath 'cppsearch'
$cssearchPath = Join-Path $csharpPath 'cssearch'
$dartsearchPath = Join-Path $dartPath 'dartsearch'
$fssearchPath = Join-Path $fsharpPath 'fssearch'
$gosearchPath = Join-Path $goPath 'gosearch'
$hssearchPath = Join-Path $haskellPath 'hssearch'
$javasearchPath = Join-Path $javaPath 'javasearch'
$jssearchPath = Join-Path $javascriptPath 'jssearch'
$ktsearchPath = Join-Path $kotlinPath 'ktsearch'
$objcsearchPath = Join-Path $objcPath 'objcsearch'
# $mlsearchPath = Join-Path $ocamlPath 'mlsearch'
$phpsearchPath = Join-Path $phpPath 'phpsearch'
$plsearchPath = Join-Path $perlPath 'plsearch'
# $ps1searchPath = Join-Path $powershellPath 'ps1search'
$pysearchPath = Join-Path $pythonPath 'pysearch'
$rbsearchPath = Join-Path $rubyPath 'rbsearch'
$rssearchPath = Join-Path $rustPath 'rssearch'
$scalasearchPath = Join-Path $scalaPath 'scalasearch'
$swiftsearchPath = Join-Path $swiftPath 'swiftsearch'
$tssearchPath = Join-Path $typescriptPath 'tssearch'
