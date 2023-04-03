################################################################################
#
# config.sh
#
# The common configuration for bash scripts
#
################################################################################

########################################
# Configuration
########################################

# XSEARCH_PATH defaults to $HOME/src/xsearch if not defined as env var
if [ -z "$XSEARCH_PATH" ]; then
    if [ -n "$XSEARCHPATH" ]; then
        XSEARCH_PATH=$XSEARCHPATH
    else
        XSEARCH_PATH=$HOME/src/xsearch
    fi
fi
BIN_PATH=$XSEARCH_PATH/bin
SHARED_PATH=$XSEARCH_PATH/shared
TEST_FILE_PATH=$SHARED_PATH/testFiles

# Language roots
C_PATH=$XSEARCH_PATH/c
CLOJURE_PATH=$XSEARCH_PATH/clojure
CPP_PATH=$XSEARCH_PATH/cpp
CSHARP_PATH=$XSEARCH_PATH/csharp
DART_PATH=$XSEARCH_PATH/dart
FSHARP_PATH=$XSEARCH_PATH/fsharp
GO_PATH=$XSEARCH_PATH/go
HASKELL_PATH=$XSEARCH_PATH/haskell
JAVA_PATH=$XSEARCH_PATH/java
JAVASCRIPT_PATH=$XSEARCH_PATH/javascript
KOTLIN_PATH=$XSEARCH_PATH/kotlin
OBJC_PATH=$XSEARCH_PATH/objc
OCAML_PATH=$XSEARCH_PATH/ocaml
PERL_PATH=$XSEARCH_PATH/perl
PHP_PATH=$XSEARCH_PATH/php
PYTHON_PATH=$XSEARCH_PATH/python
RUBY_PATH=$XSEARCH_PATH/ruby
RUST_PATH=$XSEARCH_PATH/rust
SCALA_PATH=$XSEARCH_PATH/scala
SWIFT_PATH=$XSEARCH_PATH/swift
TYPESCRIPT_PATH=$XSEARCH_PATH/typescript

# Language version roots
CSEARCH_PATH=$C_PATH/csearch
CLJSEARCH_PATH=$CLOJURE_PATH/cljsearch
CPPSEARCH_PATH=$CPP_PATH/cppsearch
CSSEARCH_PATH=$CSHARP_PATH/cssearch
DARTSEARCH_PATH=$DART_PATH/dartsearch
FSSEARCH_PATH=$FSHARP_PATH/fssearch
GOSEARCH_PATH=$GO_PATH/gosearch
HSSEARCH_PATH=$HASKELL_PATH/hssearch
JAVASEARCH_PATH=$JAVA_PATH/javasearch
JSSEARCH_PATH=$JAVASCRIPT_PATH/jssearch
KTSEARCH_PATH=$KOTLIN_PATH/ktsearch
OBJCSEARCH_PATH=$OBJC_PATH/objcsearch
OCAMLSEARCH_PATH=$OCAML_PATH/mlsearch
MLSEARCH_PATH=$OCAML_PATH/mlsearch
PLSEARCH_PATH=$PERL_PATH/plsearch
PHPSEARCH_PATH=$PHP_PATH/phpsearch
PYSEARCH_PATH=$PYTHON_PATH/pysearch
RBSEARCH_PATH=$RUBY_PATH/rbsearch
RSSEARCH_PATH=$RUST_PATH/rssearch
SCALASEARCH_PATH=$SCALA_PATH/scalasearch
SWIFTSEARCH_PATH=$SWIFT_PATH/swiftsearch
TSSEARCH_PATH=$TYPESCRIPT_PATH/tssearch
