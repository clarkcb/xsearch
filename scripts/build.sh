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

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"


########################################
# Utility Functions
########################################

# copy_resources
copy_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/config.json $resources_path/"
    cp $SHARED_PATH/config.json $resources_path/
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

build_cpp () {
    echo
    log "build_cpp"
    CPPSEARCH_PATH=$CPP_PATH/cppsearch
    log "cd $CPPSEARCH_PATH"
    cd $CPPSEARCH_PATH

    # clean
    log "/usr/local/bin/cmake --build cmake-build-debug --target clean -- -W -Wall -Werror"
    /usr/local/bin/cmake --build cmake-build-debug --target clean -- -W -Wall -Werror

    # build
    log "/usr/local/bin/cmake --build cmake-build-debug --target cppsearch -- -W -Wall -Werror"
    /usr/local/bin/cmake --build cmake-build-debug --target cppsearch -- -W -Wall -Werror

    cd -
}

build_clojure () {
    echo
    log "build_clojure"
    CLJSEARCH_PATH=$CLOJURE_PATH/cljsearch
    RESOURCES_PATH=$CLJSEARCH_PATH/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
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
    RESOURCES_PATH=$CSHARP_PATH/CsSearch/CsSearch/Resources
    #CONFIGURATION=Debug
    CONFIGURATION=Release

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # run a mono xbuild
    log "Building cssearch"
    log "xbuild /p:Configuration=$CONFIGURATION $CSHARP_PATH/CsSearch/CsSearch.sln"
    xbuild /p:Configuration=$CONFIGURATION $CSHARP_PATH/CsSearch/CsSearch.sln

}

build_fsharp () {
    echo
    log "build_fsharp"
    RESOURCES_PATH=$FSHARP_PATH/FsSearch/Resources
    CONFIGURATION=Debug
    #CONFIGURATION=Release

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # run a mono xbuild
    log "Building fssearch"
    log "xbuild /p:Configuration=$CONFIGURATION $FSHARP_PATH/FsSearch.sln"
    xbuild /p:Configuration=$CONFIGURATION $FSHARP_PATH/FsSearch.sln
}

build_go () {
    echo
    log "build_go"
    export GOPATH=$GO_PATH
    export PATH=$GOPATH/bin:$PATH

    # build the code to generate the dynamic code for gosearch
    log "Building gengosearchcode"
    echo "go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode"
    go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode

    # run it to generate the dynamic gosearch code
    log "Running gengosearchcode"
    log "gengosearchcode"
    gengosearchcode

    # go fmt the gosearch source (for auto-generated code)
    log "Auto-formatting gosearch"
    log "go fmt elocale.com/clarkcb/xsearch"
    go fmt elocale.com/clarkcb/xsearch

    # now build gosearch
    log "Building gosearch"
    log "go install elocale.com/clarkcb/xsearch/gosearch"
    go install elocale.com/clarkcb/xsearch/gosearch
}

build_haskell () {
    echo
    log "build_haskell"
    HSSEARCH_PATH=$HASKELL_PATH/hssearch
    SANDBOX_PATH=$HSSEARCH_PATH/.cabal-sandbox
    RESOURCES_PATH=$HSSEARCH_PATH/data

    if [ ! -d "$SANDBOX_PATH" ]; then
        echo "$SANDBOX_PATH not found, initializing and installing dependencies"
        cd $HSSEARCH_PATH/
        cabal sandbox init --sandbox $SANDBOX_PATH
        cd -
    fi
    if [ -d "$SANDBOX_PATH" ]; then
        cd $HSSEARCH_PATH/
        cabal install --only-dependencies
        cd -
    fi

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # build with cabal
    log "Building hssearch"
    log "cabal build"
    cd $HSSEARCH_PATH/; cabal build; cd -
}

build_java () {
    echo
    log "build_java"
    JAVASEARCH_PATH=$JAVA_PATH/javasearch
    RESOURCES_PATH=$JAVASEARCH_PATH/src/main/resources
    TEST_RESOURCES_PATH=$JAVASEARCH_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building javasearch"
    log "mvn -f $JAVASEARCH_PATH/pom.xml clean package"
    mvn -f $JAVASEARCH_PATH/pom.xml clean package
}

build_javascript () {
    echo
    log "build_javascript"
    JSSEARCH_PATH=$JAVASCRIPT_PATH/jssearch
    cd $JSSEARCH_PATH
    npm install
    gulp clean build
    cd -
}

build_kotlin () {
    echo
    log "build_kotlin"
    KTSEARCH_PATH=$KOTLIN_PATH/ktsearch
    RESOURCES_PATH=$KTSEARCH_PATH/src/main/resources
    TEST_RESOURCES_PATH=$KTSEARCH_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building ktsearch"

    cd $KTSEARCH_PATH/
    log "gradle -b build.gradle clean jar"
    gradle -b build.gradle clean jar
    cd -
}

build_ocaml () {
    echo
    log "build_ocaml"
    MLSEARCH_PATH=$OCAML_PATH/mlsearch
    cd $MLSEARCH_PATH
    ./build.sh
    if [ -L ~/bin/mlsearch ]; then
        rm ~/bin/mlsearch
    fi
    ln -s $MLSEARCH_PATH/_build/src/mlsearch.native ~/bin/mlsearch
    cd -
}

build_perl () {
    echo
    log "build_perl"
    log "Nothing to do for perl"
}

build_php () {
    echo
    log "build_php"
    log "Nothing to do for php"
}

build_python () {
    echo
    log "build_python"
    log "Nothing to do for python"
}

build_ruby () {
    echo
    log "build_ruby"
    log "Nothing to do for ruby"
}

build_scala () {
    echo
    log "build_scala"
    SCALASEARCH_PATH=$SCALA_PATH/scalasearch
    RESOURCES_PATH=$SCALASEARCH_PATH/src/main/resources
    TEST_RESOURCES_PATH=$SCALASEARCH_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building scalasearch"
    log "mvn -f $SCALASEARCH_PATH/pom.xml clean package"
    mvn -f $SCALASEARCH_PATH/pom.xml clean package
    #mvn -f $SCALASEARCH_PATH/pom.xml -DskipTests=true clean install
}

build_swift () {
    echo
    log "build_swift"
    SWIFTSEARCH_PATH=$SWIFT_PATH/swiftsearch

    # TODO: copy resource files locally?

    # run xcodebuild
    log "Building swiftsearch"
    cd $SWIFTSEARCH_PATH
    log "xcodebuild -project swiftsearch.xcodeproj"
    xcodebuild -project swiftsearch.xcodeproj
    cd -
}

build_typescript () {
    echo
    log "build_typescript"
    TSSEARCH_PATH=$TYPESCRIPT_PATH/tssearch
    cd $TSSEARCH_PATH
    npm install
    gulp clean build
    cd -
}

build_all () {
    log "build_all"

    build_cpp

    build_clojure

    build_csharp

    build_fsharp

    build_go

    build_haskell

    build_java

    build_javascript

    build_kotlin

    build_ocaml

    build_perl

    build_php

    build_python

    build_ruby

    build_scala

    build_swift

    build_typescript
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
elif [ "$ARG" == "cpp" ]; then
    build_cpp
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
elif [ "$ARG" == "javascript" ]; then
    build_javascript
elif [ "$ARG" == "kotlin" ]; then
    build_kotlin
elif [ "$ARG" == "ocaml" ]; then
    build_ocaml
elif [ "$ARG" == "perl" ]; then
    build_perl
elif [ "$ARG" == "php" ]; then
    build_php
elif [ "$ARG" == "python" ]; then
    build_python
elif [ "$ARG" == "ruby" ]; then
    build_ruby
elif [ "$ARG" == "scala" ]; then
    build_scala
elif [ "$ARG" == "swift" ]; then
    build_swift
elif [ "$ARG" == "typescript" ]; then
    build_typescript
else
    echo "ERROR: unknown build argument: $ARG"
fi
