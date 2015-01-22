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

# copy_resources
copy_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/filetypes.xml $resources_path/"
    cp $SHARED_PATH/filetypes.xml $resources_path/
    log "cp $SHARED_PATH/searchoptions.xml $resources_path/"
    cp $SHARED_PATH/searchoptions.xml $resources_path/
}

# copy_resources
copy_test_resources () {
    local test_resources_path="$1"
    log "cp $TEST_FILE_PATH/testFile*.txt $test_resources_path/"
    cp $TEST_FILE_PATH/testFile*.txt $test_resources_path/
}

########################################
# Build Functions
########################################

build_clojure () {
    echo
    log "build_clojure"
    CLJSEARCH_PATH=$PROJECT_PATH/clojure/cljsearch
    RESOURCES_PATH=$CLJSEARCH_PATH/resources

    # copy the shared xml files to the local resource location
    copy_resources $RESOURCES_PATH

    # Create uberjar with lein
    log "Building cljsearch"
    cd $CLJSEARCH_PATH
    log "lein clean"
    lein clean
    log "lein uberjar"
    lein uberjar
    cd -
}

build_csharp () {
    echo
    log "build_csharp"
    CSHARP_PATH=$PROJECT_PATH/csharp
    RESOURCES_PATH=$CSHARP_PATH/CsSearch/CsSearch/Resources

    # copy the shared xml files to the local resource location
    copy_resources $RESOURCES_PATH

    # run a mono xbuild
    log "Building cssearch"
    log "xbuild /p:Configuration=Debug $CSHARP_PATH/CsSearch/CsSearch.sln"
    xbuild /p:Configuration=Debug $CSHARP_PATH/CsSearch/CsSearch.sln

}

build_fsharp () {
    echo
    log "build_fsharp"
    FSHARP_PATH=$PROJECT_PATH/fsharp
    RESOURCES_PATH=$FSHARP_PATH/FsSearch/Resources

    # copy the shared xml files to the local resource location
    copy_resources $RESOURCES_PATH

    # run a mono xbuild
    log "Building fssearch"
    log "xbuild /p:Configuration=Debug $FSHARP_PATH/FsSearch.sln"
    xbuild /p:Configuration=Debug $FSHARP_PATH/FsSearch.sln
}

build_go () {
    echo
    log "build_go"
    export GOPATH=$PROJECT_PATH/go
    export PATH=$GOPATH/bin:$PATH

    # build the code to generate the dynamic code for gosearch
    log "Building gengosearchcode"
    echo "go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode"
    go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode

    # run it to generate the dynamic gosearch code
    log "Running gengosearchcode"
    log "gengosearchcode"
    gengosearchcode

    # now build gosearch
    log "Building gosearch"
    log "go install elocale.com/clarkcb/xsearch/gosearch"
    go install elocale.com/clarkcb/xsearch/gosearch
}

build_haskell () {
    echo
    log "build_haskell"
    HASKELL_PATH=$PROJECT_PATH/haskell
    HSSEARCH_PATH=$HASKELL_PATH/hssearch
    RESOURCES_PATH=$HSSEARCH_PATH/data

    # copy the shared xml files to the local resource location
    copy_resources $RESOURCES_PATH

    # build with cabal
    log "Building hssearch"
    log "cabal build"
    cd $HSSEARCH_PATH/; cabal build; cd -
}

build_java () {
    echo
    log "build_java"
    JAVA_PATH=$PROJECT_PATH/java
    RESOURCES_PATH=$JAVA_PATH/src/main/resources
    TEST_RESOURCES_PATH=$JAVA_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building javasearch"
    log "mvn -f $JAVA_PATH/pom.xml clean install"
    mvn -f $JAVA_PATH/pom.xml clean install
}

build_scala () {
    echo
    log "build_scala"
    SCALA_PATH=$PROJECT_PATH/scala
    RESOURCES_PATH=$SCALA_PATH/src/main/resources
    TEST_RESOURCES_PATH=$SCALA_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building scalasearch"
    log "mvn -f $SCALA_PATH/pom.xml clean install"
    mvn -f $SCALA_PATH/pom.xml clean install
    #mvn -f $SCALA_PATH/pom.xml -DskipTests=true clean install
}

build_all () {
    log "build_all"
    
    build_clojure

    build_csharp

    build_fsharp

    build_go

    build_haskell

    build_java

    build_scala
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
    build_all
elif [ "$ARG" == "clojure" ]; then
    build_clojure
elif [ "$ARG" == "csharp" ]; then
    build_csharp
elif [ "$ARG" == "fsharp" ]; then
    build_fsharp
elif [ "$ARG" == "go" ]; then
    build_go
elif [ "$ARG" == "haskell" ]; then
    build_haskell
elif [ "$ARG" == "java" ]; then
    build_java
elif [ "$ARG" == "scala" ]; then
    build_scala
else
    echo "ERROR: unknown build argument: $ARG"
fi

