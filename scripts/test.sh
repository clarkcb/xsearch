#!/bin/sh
################################################################################
#
# test.sh
#
# Runs and times a common search across the versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"

SEARCHSTRING="Searcher"
EXTS="-x py"
DEBUG=""
MULTILINE=""
# SEARCHARCHIVES="-Z"

SEARCH_PARAMS="-s \"$SEARCHSTRING\" $EXTS $DEBUG $MULTILINE -D test $XSEARCH_PATH/python"


########################################
# Build Functions
########################################

test_lang_version () {
    local lang_version="$1"
    log "$lang_version $SEARCH_PARAMS"
    time "$lang_version" -s "$SEARCHSTRING" $EXTS $DEBUG $MULTILINE -D test "$XSEARCH_PATH/python"
}

test_clojure () {
    hdr "test_clojure"
    test_lang_version "cljsearch"
}

test_cpp () {
    hdr "test_cpp"
    test_lang_version "cppsearch"
}

test_csharp () {
    hdr "test_csharp"
    test_lang_version "cssearch"
}

test_fsharp () {
    hdr "test_fsharp"
    test_lang_version "fssearch"
}

test_go () {
    hdr "test_go"
    test_lang_version "gosearch"
}

test_haskell () {
    hdr "test_haskell"
    test_lang_version "hssearch"
}

test_java () {
    hdr "test_java"
    test_lang_version "javasearch"
}

test_javascript () {
    hdr "test_javascript"
    test_lang_version "jssearch"
}

test_kotlin () {
    hdr "test_kotlin"
    test_lang_version "ktsearch"
}

test_objc () {
    hdr "test_objc"
    test_lang_version "objcsearch"
}

test_perl () {
    hdr "test_perl"
    test_lang_version "plsearch"
}

test_php () {
    hdr "test_php"
    test_lang_version "phpsearch"
}

test_python () {
    hdr "test_python"
    test_lang_version "pysearch"
}

test_ruby () {
    hdr "test_ruby"
    test_lang_version "rbsearch"
}

test_rust () {
    hdr "test_rust"
    test_lang_version "rssearch"
}

test_scala () {
    hdr "test_scala"
    test_lang_version "scalasearch"
}

test_swift () {
    hdr "test_swift"
    test_lang_version "swiftsearch"
}

test_typescript () {
    hdr "test_typescript"
    test_lang_version "tssearch"
}

test_all () {
    hdr "test_all"

    # test_clojure

    test_cpp

    test_csharp

    test_dart

    test_fsharp

    test_go

    test_haskell

    test_java

    test_javascript

    test_kotlin

    test_objc

    # test_ocaml

    test_perl

    test_php

    test_python

    test_ruby

    test_rust

    test_scala

    test_swift

    test_typescript
}


########################################
# Test Steps
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    test_all
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    test_clojure
elif [ "$ARG" == "cpp" ]
then
    test_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    test_csharp
elif [ "$ARG" == "dart" ]
then
    test_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    test_fsharp
elif [ "$ARG" == "go" ]
then
    test_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    test_haskell
elif [ "$ARG" == "java" ]
then
    test_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    test_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    test_kotlin
elif [ "$ARG" == "objc" ]
then
    test_objc
elif [ "$ARG" == "ocaml" ]
then
    test_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    test_perl
elif [ "$ARG" == "php" ]
then
    test_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    test_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    test_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    test_rust
elif [ "$ARG" == "scala" ]
then
    test_scala
elif [ "$ARG" == "swift" ]
then
    test_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    test_typescript
else
    echo "ERROR: unknown test argument: $ARG"
fi
