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
    CSSEARCHTESTS_PATH=$CSHARP_PATH/CsSearch/CsSearchTests
    # CONFIGURATION=Debug
    CONFIGURATION=Release

    # run a mono xbuild
    log "Unit-testing cssearch"
    log "mono $CSSEARCHTESTS_PATH/bin/$CONFIGURATION/CsSearchTests.exe"
    mono $CSSEARCHTESTS_PATH/bin/$CONFIGURATION/CsSearchTests.exe
}

unittest_fsharp () {
    echo
    log "unittest_fsharp"
    RESOURCES_PATH=$FSHARP_PATH/FsSearch/Resources

    # run a mono xbuild
    log "Unit-testing fssearch"
}

unittest_go () {
    echo
    log "unittest_go"
    export GOSRC_PATH=$GO_PATH/src/elocale.com/clarkcb/xsearch

    # now build gosearch
    log "Unit-testing gosearch"
    log "go test"
    cd $GOSRC_PATH; go test; cd -
}

unittest_haskell () {
    echo
    log "unittest_haskell"
    HSSEARCH_PATH=$HASKELL_PATH/hssearch

    # build with cabal
    log "Unit-testing hssearch"
    log "cabal test"
    cd $HSSEARCH_PATH/; cabal test; cd -
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
    TESTS_PATH=$JSSEARCH_PATH/tests
    NODEUNIT_PATH=$JSSEARCH_PATH/node_modules/nodeunit
    NODEUNIT=$NODEUNIT_PATH/bin/nodeunit

    if [ ! -d $NODEUNIT_PATH ]; then
        log "nodeunit not installed, installing"
        cd $JSSEARCH_PATH
        npm install nodeunit
        cd -
    fi

    # run tests via maven
    log "Unit-testing jssearch"
    log "nodeunit $TESTS_PATH"
    $NODEUNIT $TESTS_PATH
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
    XCTEST="/Applications/Xcode.app/Contents/Developer/usr/bin/xctest"
    # TESTS="/Users/cary/Library/Developer/Xcode/DerivedData/objcsearch-ahhnhqcmbhdevtgqfhmgnnerqaln/Build/Products/Debug/objcsearch_tests.xctest"
    TESTS="$OBJCSEARCH_PATH/build/Release/objcsearch_tests.xctest"
    # Usage: xctest [-XCTest All | <TestCaseClassName/testMethodName>] <path of unit to be tested>
    log "Unit-testing objcsearch"
    $XCTEST -XCTest All $TESTS
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
    TESTS_PATH=$PERL_PATH/tests

    # run tests using Test::Simple
    log "Unit-testing plsearch"
    FILES=$(find $TESTS_PATH -name "*.pl")
    for f in ${FILES[*]}; do
        log "perl $f"
        perl $f
    done
}

unittest_php () {
    echo
    log "unittest_php"
    TESTS_PATH=$PHP_PATH/tests
    PHPUNIT="phpunit --bootstrap $PHP_PATH/phpsearch/autoload.php"

    # run tests with phpunit
    log "Unit-testing phpsearch"
    FILES=$(find $TESTS_PATH -name "*.php")
    for f in ${FILES[*]}; do
        log "phpunit $f"
        $PHPUNIT $f
    done
}

unittest_python () {
    echo
    log "unittest_python"
    TESTS_PATH=$PYTHON_PATH/tests
    VENV_PATH=$PYTHON_PATH/.env
    PYTHON=$VENV_PATH/bin/python
    export PYTHONPATH=$PYTHON_PATH:$PYTHONPATH

    # activate the virtualenv
    source $PYTHON_PATH/.env/bin/activate

    # Run the individual tests
    log "Unit-testing pysearch"
    FILES=$(find $TESTS_PATH -name "*.py")
    for f in ${FILES[*]}; do
        log "python $f"
        $PYTHON $f
    done

    # deactivate the virtualenv
    deactivate
}

unittest_ruby () {
    echo
    log "unittest_ruby"
    TESTS_PATH=$RUBY_PATH/tests

    # Run the individual tests
    log "Unit-testing rbsearch"
    FILES=$(find $TESTS_PATH -name "*.rb")
    for f in ${FILES[*]}; do
        log "ruby $f"
        ruby $f
    done
}

unittest_scala () {
    echo
    log "unittest_scala"
    SCALASEARCH_PATH=$SCALA_PATH/scalasearch

    # run tests via maven
    log "Unit-testing scalasearch"
    log "mvn -f $SCALASEARCH_PATH/pom.xml test"
    mvn -f $SCALASEARCH_PATH/pom.xml test
}

unittest_swift () {
    echo
    log "unittest_swift - currently unsupported"
}

unittest_typescript () {
    echo
    log "unittest_typescript"
    TSSEARCH_PATH=$TYPESCRIPT_PATH/tssearch
    TESTS_PATH=$TSSEARCH_PATH/tests
    NODEUNIT_PATH=$TSSEARCH_PATH/node_modules/nodeunit
    NODEUNIT=$NODEUNIT_PATH/bin/nodeunit

    cd $TSSEARCH_PATH

    # clean and build the test files
    log "gulp clean build clean-tests build-tests"
    gulp clean build clean-tests build-tests

    # run tests via maven
    log "Unit-testing tssearch"
    log "nodeunit $TESTS_PATH"
    $NODEUNIT $TESTS_PATH

    cd -
}

unittest_all () {
    log "unittest_all"
    
    unittest_clojure

    unittest_csharp

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
elif [ "$ARG" == "javascript" ]; then
    unittest_javascript
elif [ "$ARG" == "kotlin" ]; then
    unittest_kotlin
elif [ "$ARG" == "objc" ]; then
    unittest_objc
elif [ "$ARG" == "ocaml" ]; then
    unittest_ocaml
elif [ "$ARG" == "perl" ]; then
    unittest_perl
elif [ "$ARG" == "php" ]; then
    unittest_php
elif [ "$ARG" == "python" ]; then
    unittest_python
elif [ "$ARG" == "ruby" ]; then
    unittest_ruby
elif [ "$ARG" == "scala" ]; then
    unittest_scala
elif [ "$ARG" == "swift" ]; then
    unittest_swift
elif [ "$ARG" == "typescript" ]; then
    unittest_typescript
else
    echo "ERROR: unknown unittest argument: $ARG"
fi
