#!/bin/bash
################################################################################
#
# test_all.sh
#
# Runs and times a common search across the versions
#
################################################################################

########################################
# Configuration
########################################

PROJECT_PATH=~/src/git/xsearch
SHARED_PATH=$PROJECT_PATH/shared

SEARCHSTRING="Searcher"
EXTS="-x py,rb"
DEBUG=""
DOTIMING="-t"
MULTILINE=""
PRINT="-p"
SEARCHARCHIVES="-Z"

SEARCH_PARAMS="-s \"$SEARCHSTRING\" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH"


########################################
# Utility Functions
########################################

# log(msg)
log () {
    local msg="$1" dt=$(date +"%Y-%m-%d %H:%M:%S")
    echo "[$dt] $msg"
}


########################################
# Build Functions
########################################

test_csharp () {
    echo -e "\n################################################################################"
    log "test_csharp"
    log "cssearch $SEARCH_PARAMS"
    time cssearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_fsharp () {
    echo -e "\n################################################################################"
    log "test_fsharp"
    time fssearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_go () {
    echo -e "\n################################################################################"
    log "test_go"
    log "gosearch $SEARCH_PARAMS"
    time gosearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_haskell () {
    echo -e "\n################################################################################"
    log "test_haskell"
    log "hssearch $SEARCH_PARAMS"
    time hssearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_java () {
    echo -e "\n################################################################################"
    log "test_java"
    log "javasearch $SEARCH_PARAMS"
    time javasearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_node () {
    echo -e "\n################################################################################"
    log "test_node"
    log "nodesearch $SEARCH_PARAMS"
    time nodesearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_php () {
    echo -e "\n################################################################################"
    log "test_php"
    log "phpsearch.php $SEARCH_PARAMS"
    time phpsearch.php -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_python () {
    echo -e "\n################################################################################"
    log "test_python"
    log "pysearch.py $SEARCH_PARAMS"
    time pysearch.py -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_ruby () {
    echo -e "\n################################################################################"
    log "test_ruby"
    log "rbsearch.rb $SEARCH_PARAMS"
    time rbsearch.rb -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_scala () {
    echo -e "\n################################################################################"
    log "test_scala"
    SCALA_PATH=$PROJECT_PATH/scala
    TARGET_PATH=$SCALA_PATH/target
    JAVASEARCHJAR=$TARGET_PATH/javasearch-1.0-SNAPSHOT.jar
    SCALASEARCHJAR=$TARGET_PATH/scala-1.0-SNAPSHOT.jar

    # do the test search
    log "scalasearch $SEARCH_PARAMS"
    time scalasearch -s "$SEARCHSTRING" $EXTS $DEBUG $DOTIMING $MULTILINE $PRINT $SEARCHARCHIVES $PROJECT_PATH
}

test_all () {
    log "test_all"
    
    test_csharp

    #test_fsharp

    test_go

    test_haskell

    test_java

    test_node

    test_php

    test_python

    test_ruby

    test_scala
}


########################################
# Test Steps
########################################

if [ $# == 0 ]; then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]; then
    test_all
elif [ "$ARG" == "csharp" ]; then
    test_csharp
elif [ "$ARG" == "fsharp" ]; then
    test_fsharp
elif [ "$ARG" == "go" ]; then
    test_go
elif [ "$ARG" == "haskell" ]; then
    test_haskell
elif [ "$ARG" == "java" ]; then
    test_java
elif [ "$ARG" == "node" ]; then
    test_node
elif [ "$ARG" == "php" ]; then
    test_php
elif [ "$ARG" == "python" ]; then
    test_python
elif [ "$ARG" == "ruby" ]; then
    test_ruby
elif [ "$ARG" == "scala" ]; then
    test_scala
else
    echo "ERROR: unknown test argument: $ARG"
fi

