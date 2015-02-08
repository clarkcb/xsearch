#!/bin/bash
################################################################################
#
# lint.sh
#
# Run static code analysis tools
#
################################################################################

########################################
# Configuration
########################################

PROJECT_PATH=$HOME/src/git/xsearch
SHARED_PATH=$PROJECT_PATH/shared


########################################
# Utility Functions
########################################

# log(msg)
log () {
    local msg="$1" dt=$(date +"%Y-%m-%d %H:%M:%S")
    echo "[$dt] $msg"
}

########################################
# Lint Functions
########################################

lint_clojure () {
    echo
    log "lint_clojure"
    CLJSEARCH_PATH=$PROJECT_PATH/clojure/cljsearch

    # Analyze
    log "Analyzing cljsearch"
    cd $CLJSEARCH_PATH
    log "lein eastwood"
    lein eastwood
    cd -
}

lint_csharp () {
    echo
    log "lint_csharp"
    CSHARP_PATH=$PROJECT_PATH/csharp

    # Analyze
    log "Analyzing cssearch not implemented at this time"
}

lint_fsharp () {
    echo
    log "lint_fsharp"
    FSHARP_PATH=$PROJECT_PATH/fsharp

    # Analyze
    log "Analyzing fssearch not implemented at this time"
}

lint_go () {
    echo
    log "lint_go"
    export GOPATH=$PROJECT_PATH/go
    SRC_PATH=$GOPATH/src
    PACKAGE=elocale.com/clarkcb/xsearch

    # Analyze
    log "Analyzing gosearch"
    cd $SRC_PATH
    log "go vet $PACKAGE"
    go vet $PACKAGE
    cd -
}

lint_haskell () {
    echo
    log "lint_haskell"
    HASKELL_PATH=$PROJECT_PATH/haskell
    HSSEARCH_PATH=$HASKELL_PATH/hssearch
    HLINT=$HSSEARCH_PATH/.cabal-sandbox/bin/hlint

    # Analyze
    log "Analyzing hssearch"
    log "hlint $HSSEARCH_PATH"
    $HLINT $HSSEARCH_PATH
}

lint_java () {
    echo
    log "lint_java"
    JAVA_PATH=$PROJECT_PATH/java
    JAVASEARCH_PATH=$JAVA_PATH/javasearch
    JAVA7=/Library/Java/JavaVirtualMachines/jdk1.7.0_60.jdk/Contents/Home/bin/java
    TOOLS_PATH=$JAVA_PATH/tools
    CHECKSTYLE=$TOOLS_PATH/checkstyle-6.3-all.jar
    CONFIG=$TOOLS_PATH/sun_checks.xml
    #CONFIG=$TOOLS_PATH/google_checks.xml

     GREPVS=("Javadoc"
             "hides a field"
             "Line is longer than 80 characters"
             "Missing package-info.java file"
             )

    # Analyze
    log "Analyzing javasearch"
    FILES=$(find $JAVASEARCH_PATH/src -name "*.java")
    for f in ${FILES[*]}; do
        log "java -jar $CHECKSTYLE -c $CONFIG $f"
        $JAVA7 -jar $CHECKSTYLE -c $CONFIG $f | grep -v -e "Javadoc" -e "hides a field" -e "Line is longer than 80 characters" -e "Missing package-info.java file"
    done
}

lint_node () {
    echo
    log "lint_node"
    NODE_PATH=$PROJECT_PATH/node
    NODESEARCH_PATH=$NODE_PATH/nodesearch
    JSHINT=$NODE_PATH/node_modules/jshint/bin/jshint

    # Analyze
    log "Analyzing nodesearch"
    FILES=$(find $NODESEARCH_PATH -name "*.js")
    for f in ${FILES[*]}; do
        log "$JSHINT $f"
        $JSHINT $f
    done
}

lint_perl () {
    echo
    log "lint_perl"
    PERL_PATH=$PROJECT_PATH/perl

    # Analyze
    log "Analyzing plsearch.pl not implemented at this time"
    #cd $PERL_PATH
    #cd -
}

lint_php () {
    echo
    log "lint_php"
    #PHP_PATH=$PROJECT_PATH/php
    #PHPSEARCH_PATH=$PHP_PATH/phpsearch
    #PHPLINT=$PHP_PATH/tools/phplint-2.0_20141127/phpl
    ## Analyze
    #log "Analyzing phpsearch.php"
    #FILES=$(find $PHPSEARCH_PATH -name "*.php")
    #for f in ${FILES[*]}; do
    #    echo "$PHPLINT $f"
    #    $PHPLINT $f
    #done
    log "Analyzing phpsearch.php not implemented at this time"
}

lint_python () {
    echo
    log "lint_python"
    PYTHON_PATH=$PROJECT_PATH/python

    # Analyze
    log "Analyzing pysearch.py"
    cd $PYTHON_PATH
    log "pylint pysearch"
    pylint pysearch
    cd -
}

lint_ruby () {
    echo
    log "lint_ruby"
    RUBY_PATH=$PROJECT_PATH/ruby
    RBSEARCH_PATH=$RUBY_PATH/rbsearch

    # Analyze
    log "Analyzing rbsearch.rb"
    FILES=$(find $RBSEARCH_PATH -name "*.rb")
    for f in ${FILES[*]}; do
        log "ruby-lint $f"
        ruby-lint $f | grep -v 'undefined'
    done
}

lint_scala () {
    echo
    log "lint_scala"
    SCALA_PATH=$PROJECT_PATH/scala

    # Analyze
    log "Analyzing scalasearch not implemented at this time"
}

lint_all () {
    log "lint_all"
    
    lint_clojure

    lint_csharp

    lint_fsharp

    lint_go

    lint_haskell

    lint_java

    lint_node

    lint_perl

    lint_php

    lint_python

    lint_ruby

    lint_scala
}


########################################
# Lint Steps
########################################

if [ $# == 0 ]; then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]; then
    lint_all
elif [ "$ARG" == "clojure" ]; then
    lint_clojure
elif [ "$ARG" == "csharp" ]; then
    lint_csharp
elif [ "$ARG" == "fsharp" ]; then
    lint_fsharp
elif [ "$ARG" == "go" ]; then
    lint_go
elif [ "$ARG" == "haskell" ]; then
    lint_haskell
elif [ "$ARG" == "java" ]; then
    lint_java
elif [ "$ARG" == "node" ]; then
    lint_node
elif [ "$ARG" == "perl" ]; then
    lint_perl
elif [ "$ARG" == "php" ]; then
    lint_php
elif [ "$ARG" == "python" ]; then
    lint_python
elif [ "$ARG" == "ruby" ]; then
    lint_ruby
elif [ "$ARG" == "scala" ]; then
    lint_scala
else
    echo "ERROR: unknown lint argument: $ARG"
fi

