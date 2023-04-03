#!/bin/sh
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
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | langcode}\n"
    exit
}


########################################
# Unit Test Functions
########################################

unittest_c () {
    echo
    hdr "unittest_c"

    # ensure make is installed
    if [ -z "$(which make)" ]
    then
        echo "You need to install make"
        return
    fi

    log "Unit-testing cfind"
    cd "$CFIND_PATH"
    log "make run_tests"
    make run_tests
    cd -
}

unittest_clojure () {
    echo
    hdr "unittest_clojure"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install lein"
        return
    fi

    # Test with lein
    log "Unit-testing cljsearch"
    cd "$CLJSEARCH_PATH"
    log "lein test"
    lein test
    cd -
}

unittest_cpp () {
    echo
    hdr "unittest_cpp"

    log "Unit-testing cppsearch"
    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$CPPSEARCH_PATH/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            CPPSEARCH_TEST_EXE=$CMAKE_BUILD_DIR/cppsearch-tests
            log "$CPPSEARCH_TEST_EXE"
            $CPPSEARCH_TEST_EXE
        fi
    done
}

unittest_csharp () {
    echo
    hdr "unittest_csharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    # VERBOSITY=quiet
    # VERBOSITY=minimal
    VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing cssearch"
    log "dotnet test $CSSEARCH_PATH/CsSearch.sln --verbosity $VERBOSITY"
    dotnet test "$CSSEARCH_PATH/CsSearch.sln" --verbosity $VERBOSITY
}

unittest_dart () {
    echo
    hdr "unittest_dart"

    cd "$DARTSEARCH_PATH"
    log "Unit-testing dartsearch"
    log "dart run test"
    dart run test
    cd -
}

unittest_fsharp () {
    echo
    hdr "unittest_fsharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    # VERBOSITY=quiet
    # VERBOSITY=minimal
    VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing fssearch"
    log "dotnet test $FSSEARCH_PATH/FsSearch.sln --verbosity $VERBOSITY"
    dotnet test "$FSSEARCH_PATH/FsSearch.sln" --verbosity $VERBOSITY
}

unittest_go () {
    echo
    hdr "unittest_go"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

    # Run the tests using go test
    log "Unit-testing gosearch"
    cd "$GOSEARCH_PATH"
    log "go test --cover ./..."
    # cd $GOSRC_PATH; go test; cd -
    go test --cover ./...
    cd -
}

unittest_haskell () {
    echo
    hdr "unittest_haskell"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

    # test with stack
    log "Unit-testing hssearch"
    log "stack test"
    cd "$HSSEARCH_PATH/"; stack test; cd -
}

unittest_java () {
    echo
    hdr "unittest_java"

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        echo "You need to install mvn"
        return
    fi

    # run tests via maven
    log "Unit-testing javasearch"
    log "mvn -f $JAVASEARCH_PATH/pom.xml test"
    mvn -f "$JAVASEARCH_PATH/pom.xml" test
}

unittest_javascript () {
    echo
    hdr "unittest_javascript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

    # run tests
    log "Unit-testing jssearch"
    cd "$JSSEARCH_PATH"
    log "npm test"
    npm test
    cd -
}

unittest_kotlin () {
    echo
    hdr "unittest_kotlin"

    # ensure gradle is installed
    if [ -z "$(which gradle)" ]
    then
        echo "You need to install gradle"
        return
    fi

    cd "$KTSEARCH_PATH"
    # run tests via gradle
    log "Unit-testing ktsearch"
    log "gradle --warning-mode all test"
    gradle --warning-mode all test
    cd -
}

unittest_objc () {
    echo
    hdr "unittest_objc"

    # ensure xcode is installed
    if [ -z "$(which xcodebuild)" ]
    then
        echo "You need to install xcode"
        return
    fi

    cd "$OBJCSEARCH_PATH"
    log "Unit-testing objcsearch"
    log "xcodebuild test -project objcsearch.xcodeproj -scheme objcsearch_tests"
    xcodebuild test -project objcsearch.xcodeproj -scheme objcsearch_tests
    cd -
}

unittest_ocaml () {
    echo
    hdr "unittest_ocaml"

    cd "$MLSEARCH_PATH"
    log "Unit-testing mlsearch"
    ./unittest.sh
    cd -
}

unittest_perl () {
    echo
    hdr "unittest_perl"

    TESTS_PATH="$PLSEARCH_PATH/t"

    # run tests using Test::Simple
    log "Unit-testing plsearch"
    FILES=$(find "$TESTS_PATH" -name "*_test.pl" | sort)
    for f in ${FILES[*]}
    do
        log "perl $f"
        perl "$f"
    done
}

unittest_php () {
    echo
    hdr "unittest_php"

    TESTS_PATH="$PHPSEARCH_PATH/tests"
    PHPUNIT="$PHPSEARCH_PATH/vendor/bin/phpunit"

    if [ ! -f "$PHPUNIT" ]
    then
        echo "You need to install phpunit first"
        return
    fi

    # run tests with phpunit
    log "Unit-testing phpsearch"
    log "$PHPUNIT $TESTS_PATH"
    "$PHPUNIT" "$TESTS_PATH"
}

unittest_python () {
    echo
    hdr "unittest_python"

    TESTS_PATH="$PYSEARCH_PATH/tests"
    VENV_PATH="$PYSEARCH_PATH/venv"
    PYTHON="$VENV_PATH/bin/python"
    export PYTHONPATH="$PYTHON_PATH"

    if [ ! -d "$VENV_PATH" ]
    then
        log "venv path not found, you probably need to run the python build (./build.sh python)"
        return
    fi

    cd "$PYSEARCH_PATH"

    # activate the virtualenv
    log "source $VENV_PATH/bin/activate"
    source "$VENV_PATH/bin/activate"

    # Run the individual tests
    log "Unit-testing pysearch"
    log "pytest"
    pytest

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    cd -
}

unittest_ruby () {
    echo
    hdr "unittest_ruby"

    log "Unit-testing rbsearch"

    # ensure rake is installed
    if [ -z "$(which rake)" ]
    then
        echo "You need to install rake"
        return
    fi

    # Run all tests via rake
    cd "$RBSEARCH_PATH"
    log "rake test"
    rake test
    cd -
}

unittest_rust () {
    echo
    hdr "unittest_rust"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        echo "You need to install cargo"
        return
    fi

    # Run cargo test
    log "Unit-testing rssearch"
    cd "$RSSEARCH_PATH"
    log "cargo test"
    cargo test
    cd -
}

unittest_scala () {
    echo
    hdr "unittest_scala"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        echo "You need to install sbt"
        return
    fi

    # run tests via sbt
    cd "$SCALASEARCH_PATH"
    log "Unit-testing scalasearch"
    log "sbt test"
    sbt test
    cd -
}

unittest_swift () {
    echo
    hdr "unittest_swift"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        echo "You need to install swift"
        return
    fi

    log "Unit-testing swiftsearch"
    cd "$SWIFTSEARCH_PATH"
    log "swift test"
    swift test
    cd -
}

unittest_typescript () {
    echo
    hdr "unittest_typescript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

    # run tests
    log "Unit-testing tssearch"
    cd "$TSSEARCH_PATH"
    log "npm test"
    npm test
    cd -
}

unittest_all () {
    hdr "unittest_all"

    # unittest_c

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

    unittest_objc

    # unittest_ocaml

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
HELP=
ARG=all

if [ $# == 0 ]
then
    HELP=yes
fi

while [ -n "$1" ]
do
    case "$1" in
        -h | --help)
            HELP=yes
            ;;
        *)
            ARG=$1
            ;;
    esac
    shift || true
done

if [ -n "$HELP" ]
then
    usage
fi

case $ARG in
    all)
        unittest_all
        ;;
    # linux)
    #     unittest_linux
    #     ;;
    # c)
    #     unittest_c
    #     ;;
    clj | clojure)
        unittest_clojure
        ;;
    cpp)
        unittest_cpp
        ;;
    cs | csharp)
        unittest_csharp
        ;;
    dart)
        unittest_dart
        ;;
    fs | fsharp)
        unittest_fsharp
        ;;
    go)
        unittest_go
        ;;
    haskell | hs)
        unittest_haskell
        ;;
    java)
        unittest_java
        ;;
    javascript | js)
        unittest_javascript
        ;;
    kotlin | kt)
        unittest_kotlin
        ;;
    objc)
        unittest_objc
        ;;
    # ocaml | ml)
    #     unittest_ocaml
    #     ;;
    perl | pl)
        unittest_perl
        ;;
    php)
        unittest_php
        ;;
    py | python)
        unittest_python
        ;;
    rb | ruby)
        unittest_ruby
        ;;
    rs | rust)
        unittest_rust
        ;;
    scala)
        unittest_scala
        ;;
    swift)
        unittest_swift
        ;;
    ts | typescript)
        unittest_typescript
        ;;
    *)
        echo -n "ERROR: unknown unittest argument: $ARG"
        ;;
esac
