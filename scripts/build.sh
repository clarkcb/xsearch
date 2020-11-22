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
    log "cp $SHARED_PATH/filetypes.json $resources_path/"
    cp $SHARED_PATH/filetypes.json $resources_path/
    log "cp $SHARED_PATH/filetypes.xml $resources_path/"
    cp $SHARED_PATH/filetypes.xml $resources_path/
    log "cp $SHARED_PATH/searchoptions.json $resources_path/"
    cp $SHARED_PATH/searchoptions.json $resources_path/
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

build_c () {
    echo
    log "build_c"
    CSEARCH_PATH=$C_PATH/csearch
    log "cd $CSEARCH_PATH"
    cd $CSEARCH_PATH

    # clean
    log "make clean"
    make clean

    # make
    log "make"
    make

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

build_csharp () {
    echo
    log "build_csharp"
    CSSEARCH_PATH=$CSHARP_PATH/CsSearch
    RESOURCES_PATH=$CSSEARCH_PATH/CsSearch/Resources
    TEST_RESOURCES_PATH=$CSSEARCH_PATH/CsSearchTests/Resources
    CONFIGURATIONS=(Debug Release)

    # copy the shared json, xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the shared test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run dotnet build for both configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building cssearch for $c configuration"
        log "dotnet build $CSSEARCH_PATH/CsSearch.sln --configuration $c"
        dotnet build $CSSEARCH_PATH/CsSearch.sln --configuration $c
    done
}

build_fsharp () {
    echo
    log "build_fsharp"
    FSSEARCH_PATH=$FSHARP_PATH/FsSearch
    RESOURCES_PATH=$FSSEARCH_PATH/FsSearch/Resources
    TEST_RESOURCES_PATH=$FSSEARCH_PATH/FsSearchTests/Resources
    CONFIGURATIONS=(Debug Release)

    # copy the shared json, xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the shared test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run dotnet for both configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building fssearch for $c configuration"
        log "dotnet build $FSSEARCH_PATH/FsSearch.sln --configuration $c"
        dotnet build $FSSEARCH_PATH/FsSearch.sln --configuration $c
    done
}

build_go () {
    echo
    log "build_go"
    export GOSEARCH_PATH=$GO_PATH/gosearch

    # build the code to generate the dynamic code for gosearch
    #log "Building gengosearchcode"
    #echo "go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode"
    #go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode

    # run it to generate the dynamic gosearch code
    #log "Running gengosearchcode"
    #log "gengosearchcode"
    #gengosearchcode

    # go fmt the gosearch source (for auto-generated code)
    log "Auto-formatting gosearch"
    cd $GOSEARCH_PATH
    log "go fmt ./..."
    go fmt ./...

    # now build gosearch
    log "Building gosearch"
    log "go install ./..."
    go install ./...
    cd -
}

build_haskell () {
    echo
    log "build_haskell"
    HSSEARCH_PATH=$HASKELL_PATH/hssearch
    RESOURCES_PATH=$HSSEARCH_PATH/data

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # build with stack (via make)
    log "Building hssearch"
    cd $HSSEARCH_PATH/
    log "stack setup"
    make setup
    log "stack build"
    make build
    log "stack install"
    stack install
    cd -
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

build_objc () {
    echo
    log "build_objc"
    OBJCSEARCH_PATH=$OBJC_PATH/objcsearch

    # TODO: copy resource files locally?

    # run xcodebuild
    log "Building objcsearch"
    cd $OBJCSEARCH_PATH
    # log "xcodebuild -project objcsearch.xcodeproj"
    # xcodebuild -project objcsearch.xcodeproj
    log "xcodebuild -alltargets"
    xcodebuild -alltargets
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
    PHPSEARCH_PATH=$PHP_PATH/phpsearch
    CONFIG_PATH=$PHPSEARCH_PATH/config
    RESOURCES_PATH=$PHPSEARCH_PATH/resources

    # copy the shared config json file to the local config location
    mkdir -p $CONFIG_PATH
    log "cp $SHARED_PATH/config.json $CONFIG_PATH/"
    cp $SHARED_PATH/config.json $CONFIG_PATH/

    # copy the shared json files to the local resource location
    mkdir -p $RESOURCES_PATH
    log "cp $SHARED_PATH/filetypes.json $RESOURCES_PATH/"
    cp $SHARED_PATH/filetypes.json $RESOURCES_PATH/
    log "cp $SHARED_PATH/searchoptions.json $RESOURCES_PATH/"
    cp $SHARED_PATH/searchoptions.json $RESOURCES_PATH/

    COMPOSER=$(which composer)
    if [ -z "$COMPOSER" ]; then
        echo "Need to install composer to continue"
        return
    fi

    # run a composer build
    log "Building phpsearch"

    cd $PHPSEARCH_PATH/
    if [ -d "$PHPSEARCH_PATH/vendor" ]; then
        log "composer update"
        composer update
    else
        log "composer install"
        composer install
    fi

    cd -
}

build_python () {
    echo
    log "build_python"
    PYSEARCH_PATH=$PYTHON_PATH/pysearch
    RESOURCES_PATH=$PYSEARCH_PATH/data

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH
}

build_ruby () {
    echo
    log "build_ruby"
    RBSEARCH_PATH=$RUBY_PATH/rbsearch
    RESOURCES_PATH=$RBSEARCH_PATH/data
    TEST_RESOURCES_PATH=$RBSEARCH_PATH/lib/test/fixtures

    # copy the shared json files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_resources $RESOURCES_PATH

    # copy the shared test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH
}

build_rust () {
    echo
    log "build_rust"
    RSSEARCH_PATH=$RUST_PATH/rssearch
    cd $RSSEARCH_PATH
    log "cargo build"
    cargo build
    log "cargo build --release"
    cargo build --release
    cd -
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

    # run sbt assembly
    cd $SCALASEARCH_PATH
    log "Building scalasearch"
    log "sbt assembly"
    sbt assembly
    cd -
}

build_swift () {
    echo
    log "build_swift"
    SWIFTSEARCH_PATH=$SWIFT_PATH/swiftsearch
    # CONFIGURATIONS=(debug release)

    # TODO: copy resource files locally?

    # run swift build
    log "Building swiftsearch"
    cd $SWIFTSEARCH_PATH
    log "swift build"
    swift build
    log "swift build --configuration release"
    swift build --configuration release
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

    build_objc

    build_ocaml

    build_perl

    build_php

    build_python

    build_ruby

    build_rust

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
elif [ "$ARG" == "c" ]; then
    build_c
elif [ "$ARG" == "clojure" ]; then
    build_clojure
elif [ "$ARG" == "cpp" ]; then
    build_cpp
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
elif [ "$ARG" == "objc" ]; then
    build_objc
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
elif [ "$ARG" == "rust" ]; then
    build_rust
elif [ "$ARG" == "scala" ]; then
    build_scala
elif [ "$ARG" == "swift" ]; then
    build_swift
elif [ "$ARG" == "typescript" ]; then
    build_typescript
else
    echo "ERROR: unknown build argument: $ARG"
fi
