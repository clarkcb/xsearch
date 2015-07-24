#!/bin/bash
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
        lines=$(($lines + ${ARR[0]}))
        words=$(($words + ${ARR[1]}))
        chars=$(($chars + ${ARR[2]}))
    done
    log "lines: $lines"
    log "words: $words"
    log "chars: $chars"
}

########################################
# Build Functions
########################################

stats_clojure () {
    echo
    log "stats_clojure"
    CLJSEARCH_SRCPATH=$XSEARCH_PATH/clojure/cljsearch/src
    CLJFILES=$(find $CLJSEARCH_SRCPATH -name "*.clj")
    log "Main source counts"
    word_counts $CLJFILES
    CLJSEARCH_TESTPATH=$XSEARCH_PATH/clojure/cljsearch/test
    CLJFILES=$(find $CLJSEARCH_TESTPATH -name "*.clj")
    log "Test counts"
    word_counts $CLJFILES
}

stats_csharp () {
    echo
    log "stats_csharp"
    CSSEARCH_SRCPATH=$CSHARP_PATH/CsSearch/CsSearch
    CSFILES=$(find $CSSEARCH_SRCPATH -name "*.cs" | grep -v /obj/)
    log "Main source counts"
    word_counts $CSFILES
    CSSEARCH_TESTPATH=$CSHARP_PATH/CsSearch/CsSearchTests
    CSFILES=$(find $CSSEARCH_TESTPATH -name "*.cs" | grep -v /obj/)
    log "Test source counts"
    word_counts $CSFILES
}

stats_fsharp () {
    echo
    log "stats_fsharp"
    FSSEARCH_SRCPATH=$FSHARP_PATH/FsSearch
    FSFILES=$(find $FSSEARCH_SRCPATH -name "*.fs")
    log "Main source counts"
    word_counts $FSFILES
}

stats_go () {
    echo
    log "stats_go"
    GOSEARCH_PATH=$GO_PATH/src/elocale.com/clarkcb/xsearch
    GOSRCFILES=$(find $GOSEARCH_PATH -name "*.go" | grep -v "_test")
    log "Main source counts"
    word_counts $GOSRCFILES
    GOTESTFILES=$(find $GOSEARCH_PATH -name "*_test.go")
    log "Test source counts"
    word_counts $GOTESTFILES
}

stats_haskell () {
    echo
    log "stats_haskell"
    HSSEARCH_SRCPATH=$HASKELL_PATH/hssearch/src
    # HSFILES=$(find $HSSEARCH_SRCPATH -name "*.hs" | grep -v "sandbox" | grep -v /dist/)
    HSSRCFILES=$(find $HSSEARCH_SRCPATH -name "*.hs")
    log "Main source counts"
    word_counts $HSSRCFILES
    HSSEARCH_TESTPATH=$HASKELL_PATH/hssearch/test
    HSTESTFILES=$(find $HSSEARCH_TESTPATH -name "*.hs")
    log "Test source counts"
    word_counts $HSTESTFILES
}

stats_java () {
    echo
    log "stats_java"
    JAVASEARCH_SRCPATH=$JAVA_PATH/javasearch/src/main
    JAVASRCFILES=$(find $JAVASEARCH_SRCPATH -name "*.java")
    log "Main source counts"
    word_counts $JAVASRCFILES
    JAVASEARCH_TESTPATH=$JAVA_PATH/javasearch/src/test
    JAVATESTFILES=$(find $JAVASEARCH_TESTPATH -name "*.java")
    log "Test source counts"
    word_counts $JAVATESTFILES
}

stats_node () {
    echo
    log "stats_node"
    NODESEARCH_PATH=$NODE_PATH/nodesearch
    JSFILES=$(find $NODESEARCH_PATH -name "*.js")
    log "Main source counts"
    word_counts $JSFILES
    TEST_PATH=$NODE_PATH/tests
    JSTESTFILES=$(find $TEST_PATH -name "*.js")
    log "Test source counts"
    word_counts $JSTESTFILES
}

stats_perl () {
    echo
    log "stats_perl"
    PLSEARCH_PATH=$PERL_PATH/plsearch
    PLSRCFILES=$(find $PLSEARCH_PATH -name "*.p[lm]")
    log "Main source counts"
    word_counts $PLSRCFILES
    TEST_PATH=$PERL_PATH/tests
    PLTESTFILES=$(find $TEST_PATH -name "*.p[lm]")
    log "Test source counts"
    word_counts $PLTESTFILES
}

stats_php () {
    echo
    log "stats_php"
    PHPSEARCH_PATH=$PHP_PATH/phpsearch
    PHPSRCFILES=$(find $PHPSEARCH_PATH -name "*.php")
    log "Main source counts"
    word_counts $PHPSRCFILES
    TEST_PATH=$PHP_PATH/tests
    PHPTESTFILES=$(find $TEST_PATH -name "*.php")
    log "Test source counts"
    word_counts $PHPTESTFILES
}

stats_python () {
    echo
    log "stats_python"
    PYSEARCH_PATH=$PYTHON_PATH/pysearch
    PYSRCFILES=$(find $PYSEARCH_PATH -name "*.py")
    log "Main source counts"
    word_counts $PYSRCFILES
    TEST_PATH=$PYTHON_PATH/tests
    PYTESTFILES=$(find $TEST_PATH -name "*.py")
    log "Test source counts"
    word_counts $PYTESTFILES
}

stats_ruby () {
    echo
    log "stats_ruby"
    RBSEARCH_PATH=$RUBY_PATH/rbsearch
    RBSRCFILES=$(find $RBSEARCH_PATH -name "*.rb")
    log "Main source counts"
    word_counts $RBSRCFILES
    TEST_PATH=$RUBY_PATH/tests
    RBTESTFILES=$(find $TEST_PATH -name "*.rb")
    log "Test source counts"
    word_counts $RBTESTFILES
}

stats_scala () {
    echo
    log "stats_scala"
    SCALASEARCH_PATH=$SCALA_PATH/scalasearch
    SCALASEARCH_SRCPATH=$SCALASEARCH_PATH/src/main
    SCALASRCFILES=$(find $SCALASEARCH_SRCPATH -name "*.scala")
    log "Main source counts"
    word_counts $SCALASRCFILES
    SCALASEARCH_TESTPATH=$SCALASEARCH_PATH/src/test
    SCALATESTFILES=$(find $SCALASEARCH_TESTPATH -name "*.scala")
    log "Test source counts"
    word_counts $SCALATESTFILES
}

stats_swift () {
    echo
    log "stats_swift"
    SWIFTSEARCH_SRCPATH=$SWIFT_PATH/swiftsearch/swiftsearch
    SWIFTSRCFILES=$(find $SWIFTSEARCH_SRCPATH -name "*.swift")
    log "Main source counts"
    word_counts $SWIFTSRCFILES
    SWIFTSEARCH_TESTPATH=$SWIFT_PATH/swiftsearch/swiftsearchTests
    SWIFTTESTFILES=$(find $SWIFTSEARCH_TESTPATH -name "*.swift")
    log "Test source counts"
    word_counts $SWIFTTESTFILES
}

stats_all () {
    log "stats_all"
    
    stats_clojure

    stats_csharp

    stats_fsharp

    stats_go

    stats_haskell

    stats_java

    stats_node

    stats_perl

    stats_php

    stats_python

    stats_ruby

    stats_scala

    stats_swift
}


########################################
# Build Steps
########################################

if [ $# == 0 ]; then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]; then
    stats_all
elif [ "$ARG" == "clojure" ]; then
    stats_clojure
elif [ "$ARG" == "csharp" ]; then
    stats_csharp
elif [ "$ARG" == "fsharp" ]; then
    stats_fsharp
elif [ "$ARG" == "go" ]; then
    stats_go
elif [ "$ARG" == "haskell" ]; then
    stats_haskell
elif [ "$ARG" == "java" ]; then
    stats_java
elif [ "$ARG" == "node" ]; then
    stats_node
elif [ "$ARG" == "perl" ]; then
    stats_perl
elif [ "$ARG" == "php" ]; then
    stats_php
elif [ "$ARG" == "python" ]; then
    stats_python
elif [ "$ARG" == "ruby" ]; then
    stats_ruby
elif [ "$ARG" == "scala" ]; then
    stats_scala
elif [ "$ARG" == "swift" ]; then
    stats_swift
else
    echo "ERROR: unknown stats argument: $ARG"
fi

