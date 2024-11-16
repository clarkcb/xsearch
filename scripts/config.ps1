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

# XFIND_PATH defaults to $HOME/src/xfind if not defined
if (-not (Test-Path env:XFIND_PATH)) { $env:XFIND_PATH = Join-Path $env:HOME 'src' 'xfind' }
$xfindPath = $env:XFIND_PATH
$xfindBinPath = Join-Path $xfindPath 'bin'
$xfindSharedPath = Join-Path $xfindPath 'shared'
$xfindTestFilePath = Join-Path $xfindSharedPath 'testFiles'

# XSEARCH_PATH defaults to $HOME/src/xsearch if not defined
if (-not (Test-Path env:XSEARCH_PATH)) { $env:XSEARCH_PATH = Join-Path $env:HOME 'src' 'xsearch' }
$xsearchPath = $env:XSEARCH_PATH
$xsearchBinPath = Join-Path $xsearchPath 'bin'
$xsearchSharedPath = Join-Path $xsearchPath 'shared'
$xsearchTestFilePath = Join-Path $xsearchSharedPath 'testFiles'

# Language roots
# $xsearchBashPath = Join-Path $xsearchPath 'bash'
# $xsearchCPath = Join-Path $xsearchPath 'c'
$xsearchClojurePath = Join-Path $xsearchPath 'clojure'
$xsearchCppPath = Join-Path $xsearchPath 'cpp'
$xsearchCsharpPath = Join-Path $xsearchPath 'csharp'
$xsearchDartPath = Join-Path $xsearchPath 'dart'
$xsearchElixirPath = Join-Path $xsearchPath 'elixir'
$xsearchFsharpPath = Join-Path $xsearchPath 'fsharp'
$xsearchGoPath = Join-Path $xsearchPath 'go'
$xsearchGroovyPath = Join-Path $xsearchPath 'groovy'
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
# $bashSearchPath = Join-Path $xsearchBashPath 'bashsearch'
# $cSearchPath = Join-Path $xsearchCPath 'csearch'
$cljSearchPath = Join-Path $xsearchClojurePath 'cljsearch'
$cppSearchPath = Join-Path $xsearchCppPath 'cppsearch'
$csSearchPath = Join-Path $xsearchCsharpPath 'cssearch'
$dartSearchPath = Join-Path $xsearchDartPath 'dartsearch'
$exSearchPath = Join-Path $xsearchElixirPath 'exsearch'
$fsSearchPath = Join-Path $xsearchFsharpPath 'fssearch'
$goSearchPath = Join-Path $xsearchGoPath 'gosearch'
$groovySearchPath = Join-Path $xsearchGroovyPath 'groovysearch'
$hsSearchPath = Join-Path $xsearchHaskellPath 'hssearch'
$javaSearchPath = Join-Path $xsearchJavaPath 'javasearch'
$jsSearchPath = Join-Path $xsearchJavascriptPath 'jssearch'
$ktSearchPath = Join-Path $xsearchKotlinPath 'ktsearch'
$objcSearchPath = Join-Path $xsearchObjcPath 'objcsearch'
# $mlSearchPath = Join-Path $xsearchOcamlPath 'mlsearch'
$phpSearchPath = Join-Path $xsearchPhpPath 'phpsearch'
$plSearchPath = Join-Path $xsearchPerlPath 'plsearch'
# $ps1SearchPath = Join-Path $xsearchPowershellPath 'ps1search'
$pySearchPath = Join-Path $xsearchPythonPath 'pysearch'
$rbSearchPath = Join-Path $xsearchRubyPath 'rbsearch'
$rsSearchPath = Join-Path $xsearchRustPath 'rssearch'
$scalaSearchPath = Join-Path $xsearchScalaPath 'scalasearch'
$swiftSearchPath = Join-Path $xsearchSwiftPath 'swiftsearch'
$tsSearchPath = Join-Path $xsearchTypescriptPath 'tssearch'
