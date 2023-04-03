#!/bin/sh
################################################################################
#
# stats.sh
#
# Get code file stats for the various languages
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"


########################################
# Utility Functions
########################################

# word_counts
word_counts () {
    local files="$@" lines=0 words=0 chars=0
    for f in ${files[*]}; do
        # log "wc $f"
        WC=$(wc $f)
        # echo "$WC"
        ARR=($WC)
        # flen=${#files[*]}
        flen=$#
        lines=$(($lines + ${ARR[0]}))
        words=$(($words + ${ARR[1]}))
        chars=$(($chars + ${ARR[2]}))
    done
    log "files: $flen"
    log "lines: $lines"
    log "words: $words"
    log "chars: $chars"
}

########################################
# Build Functions
########################################

stats_c () {
    echo
    hdr "stats_c"
    # find ./ -type f \( -iname \*.jpg -o -iname \*.png \)
    CSEARCH_INCPATH=$CSEARCH_PATH/include
    CFILES=$(find $CSEARCH_INCPATH -type f -iname "*.h")
    log "Main include counts"
    word_counts $CFILES
    CSEARCH_SRCPATH=$CSEARCH_PATH/src
    CFILES=$(find $CSEARCH_SRCPATH -type f \( -iname \*.h -o -iname \*.c \))
    log "Main source counts"
    word_counts $CFILES
    CSEARCH_TESTPATH=$CSEARCH_PATH/tests
    CFILES=$(find $CSEARCH_TESTPATH -type f -iname "*.c")
    log "Test source counts"
    word_counts $CFILES
}

stats_clojure () {
    echo
    hdr "stats_clojure"
    CLJSEARCH_SRCPATH=$CLJSEARCH_PATH/src
    CLJFILES=$(find $CLJSEARCH_SRCPATH -type f -iname "*.clj")
    log "Main source counts"
    word_counts $CLJFILES
    CLJSEARCH_TESTPATH=$CLJSEARCH_PATH/test
    CLJFILES=$(find $CLJSEARCH_TESTPATH -type f -iname "*.clj")
    log "Test counts"
    word_counts $CLJFILES
}

stats_cpp () {
    echo
    hdr "stats_cpp"
    # find ./ -type f \( -iname \*.jpg -o -iname \*.png \)
    CPPSEARCH_INCPATH=$CPPSEARCH_PATH/include
    CPPFILES=$(find $CPPSEARCH_INCPATH -type f -iname "*.h")
    log "Main include counts"
    word_counts $CPPFILES
    CPPSEARCH_SRCPATH=$CPPSEARCH_PATH/src
    CPPFILES=$(find $CPPSEARCH_SRCPATH -type f \( -iname \*.h -o -iname \*.cpp \))
    log "Main source counts"
    word_counts $CPPFILES
    CPPSEARCH_TESTPATH=$CPPSEARCH_PATH/tests
    CPPFILES=$(find $CPPSEARCH_TESTPATH -type f -iname "*.cpp")
    log "Test source counts"
    word_counts $CPPFILES
}

stats_csharp () {
    echo
    hdr "stats_csharp"
    CSSEARCH_SRCPATH=$CSSEARCH_PATH/CsSearch
    CSSEARCHLIB_SRCPATH=$CSSEARCH_PATH/CsSearchLib
    CSFILES=$(find $CSSEARCH_SRCPATH $CSSEARCHLIB_SRCPATH -type f -iname "*.cs" | grep -v /obj/)
    log "Main source counts"
    word_counts $CSFILES
    CSSEARCH_TESTPATH=$CSSEARCH_PATH/CsSearchTests
    CSFILES=$(find $CSSEARCH_TESTPATH -type f -iname "*.cs" | grep -v /obj/)
    log "Test source counts"
    word_counts $CSFILES
}

stats_dart () {
    echo
    hdr "stats_dart"
    DARTSEARCH_SRCPATH=$DARTSEARCH_PATH/lib
    DARTFILES=$(find $DARTSEARCH_SRCPATH -type f -iname "*.dart")
    log "Main source counts"
    word_counts $DARTFILES
    DARTSEARCH_TESTPATH=$DARTSEARCH_PATH/test
    DARTFILES=$(find $DARTSEARCH_TESTPATH -type f -iname "*.dart")
    log "Test source counts"
    word_counts $DARTFILES
}

stats_fsharp () {
    echo
    hdr "stats_fsharp"
    FSSEARCH_SRCPATH=$FSSEARCH_PATH/FsSearch
    FSSEARCHLIB_SRCPATH=$FSSEARCH_PATH/FsSearchLib
    FSFILES=$(find $FSSEARCH_SRCPATH $FSSEARCHLIB_SRCPATH -type f -iname "*.fs")
    log "Main source counts"
    word_counts $FSFILES
    FSSEARCH_TESTPATH=$FSSEARCH_PATH/FsSearchTests
    FSFILES=$(find $FSSEARCH_TESTPATH -type f -iname "*.fs" | grep -v /obj/)
    log "Test source counts"
    word_counts $FSFILES
}

stats_go () {
    echo
    hdr "stats_go"
    GOFILES=$(find $GOSEARCH_PATH -type f -iname "*.go" | grep -v "_test")
    log "Main source counts"
    word_counts $GOFILES
    GOFILES=$(find $GOSEARCH_PATH -type f -iname "*_test.go")
    log "Test source counts"
    word_counts $GOFILES
}

stats_haskell () {
    echo
    hdr "stats_haskell"
    HSSEARCH_SRCPATH=$HSSEARCH_PATH/src
    HSSRCFILES=$(find $HSSEARCH_SRCPATH -type f -iname "*.hs")
    log "Main source counts"
    word_counts $HSSRCFILES
    HSSEARCH_TESTPATH=$HSSEARCH_PATH/test
    HSTESTFILES=$(find $HSSEARCH_TESTPATH -type f -iname "*.hs")
    log "Test source counts"
    word_counts $HSTESTFILES
}

stats_java () {
    echo
    hdr "stats_java"
    JAVASEARCH_SRCPATH=$JAVASEARCH_PATH/src/main
    JAVASRCFILES=$(find $JAVASEARCH_SRCPATH -type f -iname "*.java")
    log "Main source counts"
    word_counts $JAVASRCFILES
    JAVASEARCH_TESTPATH=$JAVASEARCH_PATH/src/test
    JAVATESTFILES=$(find $JAVASEARCH_TESTPATH -type f -iname "*.java")
    log "Test source counts"
    word_counts $JAVATESTFILES
}

stats_javascript () {
    echo
    hdr "stats_javascript"
    JSSEARCH_SRCPATH=$JSSEARCH_PATH/src
    JSFILES=$(find $JSSEARCH_SRCPATH -type f -iname "*.js")
    log "Main source counts"
    word_counts $JSFILES
    JSSEARCH_TESTPATH=$JSSEARCH_PATH/tests
    JSTESTFILES=$(find $JSSEARCH_TESTPATH -type f -iname "*.js")
    log "Test source counts"
    word_counts $JSTESTFILES
}

stats_kotlin () {
    echo
    hdr "stats_kotlin"
    KTSEARCH_SRCPATH=$KTSEARCH_PATH/src/main
    KTFILES=$(find $KTSEARCH_SRCPATH -type f -iname "*.kt")
    log "Main source counts"
    word_counts $KTFILES
    KTSEARCH_TESTPATH=$KTSEARCH_PATH/src/test
    KTTESTFILES=$(find $KTSEARCH_TESTPATH -type f -iname "*.kt")
    log "Test source counts"
    word_counts $KTTESTFILES
}

stats_objc () {
    echo
    hdr "stats_objc"
    OBJCSEARCH_SRCPATH=$OBJCSEARCH_PATH/objcsearch
    OBJCFILES=$(find $OBJCSEARCH_SRCPATH -type f \( -iname \*.h -o -iname \*.m \))
    log "Main source counts"
    word_counts $OBJCFILES
    OBJCSEARCH_TESTPATH=$OBJCSEARCH_PATH/objcsearch_tests
    OBJCTESTFILES=$(find $OBJCSEARCH_TESTPATH -type f -iname "*.m")
    log "Test source counts"
    word_counts $OBJCTESTFILES
}

stats_ocaml () {
    echo
    hdr "stats_ocaml"
    OCAMLSEARCH_SRCPATH=$OCAMLSEARCH_PATH/src
    OCAMLFILES=$(find $OCAMLSEARCH_SRCPATH -type f \( -iname \*.ml -o -iname \*.mli \))
    log "Main source counts"
    word_counts $OCAMLFILES
    OCAMLSEARCH_TESTPATH=$OCAMLSEARCH_PATH/tests
    OCAMLFILES=$(find $OCAMLSEARCH_TESTPATH -type f -iname "*.ml")
    log "Test source counts"
    word_counts $OCAMLFILES
}

stats_perl () {
    echo
    hdr "stats_perl"
    PLSEARCH_SRCPATH=$PLSEARCH_PATH/lib
    PLSRCFILES=$(find $PLSEARCH_SRCPATH -type f -iname "*.p[lm]")
    log "Main source counts"
    word_counts $PLSRCFILES
    PLSEARCH_TESTPATH=$PLSEARCH_PATH/t
    PLTESTFILES=$(find $PLSEARCH_TESTPATH -type f -iname "*.p[lm]")
    log "Test source counts"
    word_counts $PLTESTFILES
}

stats_php () {
    echo
    hdr "stats_php"
    PHPSEARCH_SRCPATH=$PHPSEARCH_PATH/src
    PHPSRCFILES=$(find $PHPSEARCH_SRCPATH -type f -iname "*.php")
    log "Main source counts"
    word_counts $PHPSRCFILES
    PHPSEARCH_TESTPATH=$PHPSEARCH_PATH/tests
    PHPTESTFILES=$(find $PHPSEARCH_TESTPATH -type f -iname "*.php")
    log "Test source counts"
    word_counts $PHPTESTFILES
}

stats_python () {
    echo
    hdr "stats_python"
    PYSEARCH_SRCPATH=$PYSEARCH_PATH/pyfind
    PYSRCFILES=$(find $PYSEARCH_SRCPATH -type f -iname "*.py")
    log "Main source counts"
    word_counts $PYSRCFILES
    PYSEARCH_TESTPATH=$PYSEARCH_PATH/tests
    PYTESTFILES=$(find $PYSEARCH_TESTPATH -type f -iname "*.py")
    log "Test source counts"
    word_counts $PYTESTFILES
}

stats_ruby () {
    echo
    hdr "stats_ruby"
    RBSEARCH_SRCPATH=$RBSEARCH_PATH/lib
    RBSRCFILES=$(find $RBSEARCH_SRCPATH -type f -iname "*.rb")
    log "Main source counts"
    word_counts $RBSRCFILES
    RBSEARCH_TESTPATH=$RBSEARCH_PATH/test
    RBTESTFILES=$(find $RBSEARCH_TESTPATH -type f -iname "*.rb")
    log "Test source counts"
    word_counts $RBTESTFILES
}

stats_rust () {
    echo
    hdr "stats_rust"
    RSSEARCH_SRCPATH=$RSSEARCH_PATH/src
    RSSRCFILES=$(find $RSSEARCH_SRCPATH -type f -iname "*.rs")
    log "Source counts (tests are embedded)"
    word_counts $RSSRCFILES
}

stats_scala () {
    echo
    hdr "stats_scala"
    SCALASEARCH_SRCPATH=$SCALASEARCH_PATH/src/main
    SCALASRCFILES=$(find $SCALASEARCH_SRCPATH -type f -iname "*.scala")
    log "Main source counts"
    word_counts $SCALASRCFILES
    SCALASEARCH_TESTPATH=$SCALASEARCH_PATH/src/test
    SCALATESTFILES=$(find $SCALASEARCH_TESTPATH -type f -iname "*.scala")
    log "Test source counts"
    word_counts $SCALATESTFILES
}

stats_swift () {
    echo
    hdr "stats_swift"
    SWIFTSEARCH_SRCPATH=$SWIFTSEARCH_PATH/Sources
    SWIFTSRCFILES=$(find $SWIFTSEARCH_SRCPATH -type f -iname "*.swift")
    log "Main source counts"
    word_counts $SWIFTSRCFILES
    SWIFTSEARCH_TESTPATH=$SWIFTSEARCH_PATH/Tests
    SWIFTTESTFILES=$(find $SWIFTSEARCH_TESTPATH -type f -iname "*.swift")
    log "Test source counts"
    word_counts $SWIFTTESTFILES
}

stats_typescript () {
    echo
    hdr "stats_typescript"
    TSSEARCH_SRCPATH=$TSSEARCH_PATH/src
    TSFILES=$(find $TSSEARCH_SRCPATH -type f -iname "*.ts")
    log "Main source counts"
    word_counts $TSFILES
    TSSEARCH_TESTPATH=$TSSEARCH_PATH/tests
    TSTESTFILES=$(find $TSSEARCH_TESTPATH -type f -iname "*.ts")
    log "Test source counts"
    word_counts $TSTESTFILES
}

stats_all () {
    hdr "stats_all"

    # stats_c

    stats_clojure

    stats_cpp

    stats_csharp

    stats_dart

    stats_fsharp

    stats_go

    stats_haskell

    stats_java

    stats_javascript

    stats_kotlin

    stats_objc

    stats_ocaml

    stats_perl

    stats_php

    stats_python

    stats_ruby

    stats_rust

    stats_scala

    stats_swift

    stats_typescript
}


########################################
# Build Steps
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    stats_all
# elif [ "$ARG" == "c" ]
# then
#     stats_c
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    stats_clojure
elif [ "$ARG" == "cpp" ]
then
    stats_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    stats_csharp
elif [ "$ARG" == "dart" ]
then
    stats_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    stats_fsharp
elif [ "$ARG" == "go" ] || [ "$ARG" == "go" ]
then
    stats_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    stats_haskell
elif [ "$ARG" == "java" ]
then
    stats_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    stats_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    stats_kotlin
elif [ "$ARG" == "objc" ]
then
    stats_objc
elif [ "$ARG" == "ocaml" ]
then
    stats_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    stats_perl
elif [ "$ARG" == "php" ] || [ "$ARG" == "php" ]
then
    stats_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    stats_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    stats_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    stats_rust
elif [ "$ARG" == "scala" ]
then
    stats_scala
elif [ "$ARG" == "swift" ]
then
    stats_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    stats_typescript
else
    echo "ERROR: unknown stats argument: $ARG"
fi
