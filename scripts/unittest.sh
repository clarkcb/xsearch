#!/bin/bash
################################################################################
#
# unittest.sh
#
# Runs unit tests for specified language version of xsearch, or all versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"


########################################
# Unit Test Functions
########################################

unittest_clojure () {
    echo
    log "unittest_clojure"
    CLJSEARCH_PATH=$CLOJURE_PATH/cljsearch

    # Test with lein
    log "Unit-testing cljsearch"
    cd $CLJSEARCH_PATH
    log "lein test"
    lein test
    cd -
}

unittest_cpp () {
    echo
    log "unittest_cpp"
    CPPSEARCH_PATH=$CPP_PATH/cppsearch

    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$CPPSEARCH_PATH/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]; then
            CPPSEARCH_TEST_EXE=$CMAKE_BUILD_DIR/cppsearch-tests
            log "Unit-testing cppsearch"
            log "$CPPSEARCH_TEST_EXE"
            $CPPSEARCH_TEST_EXE
        fi
    done
}

unittest_dart () {
    echo
    log "unittest_dart"
    DARTSEARCH_PATH=$DART_PATH/dartsearch

    cd $DARTSEARCH_PATH
    log "Unit-testing dartsearch"
    log "pub run test"
    pub run test
    cd -
}

unittest_csharp () {
    echo
    log "unittest_csharp"
    CSSEARCH_PATH=$CSHARP_PATH/CsSearch
    # VERBOSITY=quiet
    # VERBOSITY=minimal
    VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing cssearch"
    log "dotnet test $CSSEARCH_PATH/CsSearch.sln --verbosity $VERBOSITY"
    dotnet test $CSSEARCH_PATH/CsSearch.sln --verbosity $VERBOSITY
}

unittest_fsharp () {
    echo
    log "unittest_fsharp"
    FSSEARCH_PATH=$FSHARP_PATH/FsSearch
    # VERBOSITY=quiet
    # VERBOSITY=minimal
    VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing fssearch"
    log "dotnet test $FSSEARCH_PATH/FsSearch.sln --verbosity $VERBOSITY"
    dotnet test $FSSEARCH_PATH/FsSearch.sln --verbosity $VERBOSITY
}

unittest_go () {
    echo
    log "unittest_go"
    export GOSEARCH_PATH=$GO_PATH/gosearch

    # Run the tests using go test
    log "Unit-testing gosearch"
    cd $GOSEARCH_PATH
    log "go test --cover ./..."
    # cd $GOSRC_PATH; go test; cd -
    go test --cover ./...
    cd -
}

unittest_haskell () {
    echo
    log "unittest_haskell"
    HSSEARCH_PATH=$HASKELL_PATH/hssearch

    # test with stack
    log "Unit-testing hssearch"
    log "stack test"
    cd $HSSEARCH_PATH/; stack test; cd -
}

unittest_java () {
    echo
    log "unittest_java"
    JAVASEARCH_PATH=$JAVA_PATH/javasearch

    # run tests via maven
    log "Unit-testing javasearch"
    log "mvn -f $JAVASEARCH_PATH/pom.xml test"
    mvn -f $JAVASEARCH_PATH/pom.xml test
}

unittest_javascript () {
    echo
    log "unittest_javascript"
    JSSEARCH_PATH=$JAVASCRIPT_PATH/jssearch

    # run tests
    log "Unit-testing jssearch"
    cd $JSSEARCH_PATH
    log "npm test"
    npm test
    cd -
}

unittest_kotlin () {
    echo
    log "unittest_kotlin"
    KTSEARCH_PATH=$KOTLIN_PATH/ktsearch

    # run tests via gradle
    log "Unit-testing ktsearch"
    log "gradle -b $KTSEARCH_PATH/build.gradle test"
    gradle -b $KTSEARCH_PATH/build.gradle test
}

unittest_objc () {
    echo
    log "unittest_objc"
    OBJCSEARCH_PATH=$OBJC_PATH/objcsearch
    cd $OBJCSEARCH_PATH

    log "Unit-testing objcsearch"
    log "xcodebuild test -project objcsearch.xcodeproj -scheme objcsearch_tests"
    xcodebuild test -project objcsearch.xcodeproj -scheme objcsearch_tests
    cd -
}

unittest_ocaml () {
    echo
    log "unittest_ocaml"
    MLSEARCH_PATH=$OCAML_PATH/mlsearch
    cd $MLSEARCH_PATH
    log "Unit-testing mlsearch"
    ./unittest.sh
    cd -
}

unittest_perl () {
    echo
    log "unittest_perl"
    TESTS_PATH=$PERL_PATH/plsearch/t

    # run tests using Test::Simple
    log "Unit-testing plsearch"
    FILES=$(find $TESTS_PATH -name "*_test.pl")
    for f in ${FILES[*]}; do
        log "perl $f"
        perl $f
    done
}

unittest_php () {
    echo
    log "unittest_php"
    PHPSEARCH_PATH=$PHP_PATH/phpsearch
    TESTS_PATH=$PHPSEARCH_PATH/tests
    PHPUNIT=$PHPSEARCH_PATH/vendor/bin/phpunit

    if [ ! -f "$PHPUNIT" ]; then
        echo "You need to install phpunit first"
        return
    fi

    # run tests with phpunit
    log "Unit-testing phpsearch"
    log "$PHPUNIT $TESTS_PATH"
    $PHPUNIT $TESTS_PATH
}

unittest_python () {
    echo
    log "unittest_python"
    PYSEARCH_PATH=$PYTHON_PATH/pysearch
    TESTS_PATH=$PYSEARCH_PATH/tests
    VENV_PATH=$PYSEARCH_PATH/venv
    PYTHON=$VENV_PATH/bin/python
    export PYTHONPATH=$PYTHON_PATH

    # activate the virtualenv
    source $VENV_PATH/bin/activate

    cd $PYSEARCH_PATH

    # Run the individual tests
    log "Unit-testing pysearch"
    log "nosetests"
    nosetests

    # deactivate the virtualenv
    deactivate

    cd -
}

unittest_ruby () {
    echo
    log "unittest_ruby"
    RBSEARCH_PATH=$RUBY_PATH/rbsearch

    log "Unit-testing rbsearch"

    # Run the individual tests
    # TESTS_PATH=$RBSEARCH_PATH/test
    # FILES=$(find $TESTS_PATH -name "*.rb")
    # for f in ${FILES[*]}; do
    #     log "ruby $f"
    #     ruby $f
    # done

    # Run all tests via rake
    cd $RBSEARCH_PATH
    rake test
    cd -
}

unittest_rust () {
    echo
    log "unittest_rust"
    RSSEARCH_PATH=$RUST_PATH/rssearch

    # Run cargo test
    log "Unit-testing rssearch"
    log "cargo test"
    cd $RSSEARCH_PATH
    cargo test
    cd -
}

unittest_scala () {
    echo
    log "unittest_scala"
    SCALASEARCH_PATH=$SCALA_PATH/scalasearch

    # run tests via sbt
    cd $SCALASEARCH_PATH
    log "Unit-testing scalasearch"
    log "sbt test"
    sbt test
    cd -
}

unittest_swift () {
    echo
    log "unittest_swift"
    SWIFTSEARCH_PATH=$SWIFT_PATH/swiftsearch
    cd $SWIFTSEARCH_PATH
    log "Unit-testing swiftsearch"
    log "swift test"
    swift test
    cd -
}

unittest_typescript () {
    echo
    log "unittest_typescript"
    TSSEARCH_PATH=$TYPESCRIPT_PATH/tssearch

    # run tests
    log "Unit-testing jssearch"
    cd $TSSEARCH_PATH
    log "npm test"
    npm test
    cd -
}

unittest_all () {
    log "unittest_all"
    
    unittest_clojure

    unittest_cpp

    unittest_csharp

    unittest_dart

    unittest_fsharp

    unittest_go

    unittest_haskell

    unittest_java

    unittest_javascript

    unittest_kotlin

    unittest_ocaml

    unittest_perl

    unittest_php

    unittest_python

    unittest_ruby

    unittest_rust

    unittest_scala

    unittest_swift

    unittest_typescript
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
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]; then
    unittest_clojure
elif [ "$ARG" == "cpp" ]; then
    unittest_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]; then
    unittest_csharp
elif [ "$ARG" == "dart" ]; then
    unittest_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]; then
    unittest_fsharp
elif [ "$ARG" == "go" ]; then
    unittest_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]; then
    unittest_haskell
elif [ "$ARG" == "java" ]; then
    unittest_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]; then
    unittest_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]; then
    unittest_kotlin
elif [ "$ARG" == "objc" ]; then
    unittest_objc
elif [ "$ARG" == "ocaml" ]; then
    unittest_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]; then
    unittest_perl
elif [ "$ARG" == "php" ]; then
    unittest_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]; then
    unittest_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]; then
    unittest_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]; then
    unittest_rust
elif [ "$ARG" == "scala" ]; then
    unittest_scala
elif [ "$ARG" == "swift" ]; then
    unittest_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]; then
    unittest_typescript
else
    echo "ERROR: unknown unittest argument: $ARG"
fi
