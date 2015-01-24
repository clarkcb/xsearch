#!/bin/bash
################################################################################
#
# build.sh
#
# Builds specified language version of xsearch, or all versions
#
################################################################################

########################################
# Configuration
########################################

PROJECT_PATH=/Users/cary/src/git/xsearch
SHARED_PATH=$PROJECT_PATH/shared
TEST_FILE_PATH=$SHARED_PATH/testFiles


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

unittest_clojure () {
    echo
    log "unittest_clojure"
    CLJSEARCH_PATH=$PROJECT_PATH/clojure/cljsearch
    RESOURCES_PATH=$CLJSEARCH_PATH/resources

    # Test with lein
    log "Unit-testing cljsearch"
    cd $CLJSEARCH_PATH
    log "lein test"
    lein test
    cd -
}

unittest_csharp () {
    echo
    log "unittest_csharp"
    CSHARP_PATH=$PROJECT_PATH/csharp
    RESOURCES_PATH=$CSHARP_PATH/CsSearch/CsSearch/Resources

    # run a mono xbuild
    log "Unit-testing cssearch"
    #log "xbuild /p:Configuration=Debug $CSHARP_PATH/CsSearch/CsSearch.sln"
    #xbuild /p:Configuration=Debug $CSHARP_PATH/CsSearch/CsSearch.sln

}

unittest_fsharp () {
    echo
    log "unittest_fsharp"
    FSHARP_PATH=$PROJECT_PATH/fsharp
    RESOURCES_PATH=$FSHARP_PATH/FsSearch/Resources

    # run a mono xbuild
    log "Unit-testing fssearch"
    #log "xbuild /p:Configuration=Debug $FSHARP_PATH/FsSearch.sln"
    #xbuild /p:Configuration=Debug $FSHARP_PATH/FsSearch.sln
}

unittest_go () {
    echo
    log "unittest_go"
    export GOPATH=$PROJECT_PATH/go
    export GOSRC_PATH=$GOPATH/src/elocale.com/clarkcb/xsearch

    # now build gosearch
    log "Unit-testing gosearch"
    log "go test"
    cd $GOSRC_PATH; go test; cd -
}

unittest_haskell () {
    echo
    log "unittest_haskell"
    HASKELL_PATH=$PROJECT_PATH/haskell
    HSSEARCH_PATH=$HASKELL_PATH/hssearch

    # build with cabal
    log "Unit-testing hssearch"
    #log "cabal build"
    #cd $HSSEARCH_PATH/; cabal build; cd -
}

unittest_java () {
    echo
    log "unittest_java"
    JAVA_PATH=$PROJECT_PATH/java

    # run tests via maven
    log "Unit-testing javasearch"
    log "mvn -f $JAVA_PATH/pom.xml test"
    mvn -f $JAVA_PATH/pom.xml test
}

unittest_scala () {
    echo
    log "unittest_scala"
    SCALA_PATH=$PROJECT_PATH/scala

    # run tests via maven
    log "Unit-testing scalasearch"
    log "mvn -f $SCALA_PATH/pom.xml test"
    mvn -f $SCALA_PATH/pom.xml test
}

unittest_all () {
    log "unittest_all"
    
    unittest_clojure

    unittest_csharp

    unittest_fsharp

    unittest_go

    unittest_haskell

    unittest_java

    unittest_scala
}


########################################
# Unit-testing main
########################################

if [ $# == 0 ]; then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]; then
    unittest_all
elif [ "$ARG" == "clojure" ]; then
    unittest_clojure
elif [ "$ARG" == "csharp" ]; then
    unittest_csharp
elif [ "$ARG" == "fsharp" ]; then
    unittest_fsharp
elif [ "$ARG" == "go" ]; then
    unittest_go
elif [ "$ARG" == "haskell" ]; then
    unittest_haskell
elif [ "$ARG" == "java" ]; then
    unittest_java
elif [ "$ARG" == "scala" ]; then
    unittest_scala
else
    echo "ERROR: unknown unittest argument: $ARG"
fi

