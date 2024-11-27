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

# Add failed builds to this array and report failed builds at the end
FAILED_BUILDS=()


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}

print_failed_builds () {
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed unit tests: ${FAILED_BUILDS[*]}"
    else
        log "All unit tests succeeded"
    fi
}


########################################
# Unit Test Functions
########################################

unittest_bashsearch () {
    echo
    hdr "unittest_bashsearch"
    log "Not currently implemented"
}

unittest_csearch () {
    echo
    hdr "unittest_csearch"

    # ensure make is installed
    if [ -z "$(which make)" ]
    then
        log_error "You need to install make"
        FAILED_BUILDS+=("csearch")
        return
    fi

    log "Unit-testing csearch"

    cd "$CFIND_PATH"
    log "make run_tests"
    make run_tests

    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("csearch")
    fi

    cd -
}

unittest_cljsearch () {
    echo
    hdr "unittest_cljsearch"

    # if clojure is installed, display version
    if [ -n "$(which clj)" ]
    then
        # clj -version output looks like this: Clojure CLI version 1.11.4.1474
        # CLOJURE_VERSION=$(clj -version | head -n 1 | cut -d ' ' -f 3)
        CLOJURE_VERSION=$(clj -version 2>&1)
        log "clojure version: $CLOJURE_VERSION"
    fi

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install lein"
        FAILED_BUILDS+=("cljsearch")
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    cd "$CLJSEARCH_PATH"

    # Test with lein
    log "Unit-testing cljsearch"
    log "lein test"
    lein test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("cljsearch")
    fi

    cd -
}

unittest_cppsearch () {
    echo
    hdr "unittest_cppsearch"

    # if cmake is installed, display version
    if [ -n "$(which cmake)" ]
    then
        # cmake --version output looks like this: cmake version 3.30.2
        CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
        log "cmake version: $CMAKE_VERSION"
    fi

    log "Unit-testing cppsearch"
    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$CPPSEARCH_PATH/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            CPPSEARCH_TEST_EXE=$CMAKE_BUILD_DIR/cppsearch-tests
            if [ -e "$CPPSEARCH_TEST_EXE" ]
            then
                log "$CPPSEARCH_TEST_EXE"
                $CPPSEARCH_TEST_EXE

                if [ "$?" -eq 0 ]
                then
                    log "C++ unit test for $c succeeded"
                else
                    log_error "ERROR: cppsearch unit tests for $c failed"
                    FAILED_BUILDS+=("cppsearch")
                    return
                fi
            else
                log_error "cppsearch-tests not found: $CPPSEARCH_TEST_EXE"
                FAILED_BUILDS+=("cppsearch")
                return
            fi
        else
            log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
            FAILED_BUILDS+=("cppsearch")
            return
        fi
    done
}

unittest_cssearch () {
    echo
    hdr "unittest_cssearch"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("cssearch")
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # VERBOSITY=quiet
    VERBOSITY=minimal
    # VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing cssearch"
    log "dotnet test $CSSEARCH_PATH/CsSearch.sln --verbosity $VERBOSITY"
    dotnet test "$CSSEARCH_PATH/CsSearch.sln" --verbosity $VERBOSITY

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("cssearch")
    fi
}

unittest_dartsearch () {
    echo
    hdr "unittest_dartsearch"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        FAILED_BUILDS+=("dartsearch")
        return
    fi

    DART_VERSION=$(dart --version)
    log "$DART_VERSION"

    cd "$DARTSEARCH_PATH"

    log "Unit-testing dartsearch"
    log "dart run test"
    dart run test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("dartsearch")
    fi

    cd -
}

unittest_exsearch () {
    echo
    hdr "unittest_exsearch"

    # if elixir is installed, display version
    if [ -n "$(which elixir)" ]
    then
        ELIXIR_VERSION=$(elixir --version | grep Elixir)
        log "elixir version: $ELIXIR_VERSION"
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        FAILED_BUILDS+=("exsearch")
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    cd "$EXSEARCH_PATH"

    # run tests
    log "Unit-testing exsearch"
    log "mix test"
    mix test

    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("exsearch")
    fi

    cd -
}

unittest_fssearch () {
    echo
    hdr "unittest_fssearch"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("fssearch")
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # VERBOSITY=quiet
    VERBOSITY=minimal
    # VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing fssearch"
    log "dotnet test $FSSEARCH_PATH/FsSearch.sln --verbosity $VERBOSITY"
    dotnet test "$FSSEARCH_PATH/FsSearch.sln" --verbosity $VERBOSITY

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("fssearch")
    fi
}

unittest_gosearch () {
    echo
    hdr "unittest_gosearch"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        FAILED_BUILDS+=("gosearch")
        return
    fi

    GO_VERSION=$(go version | sed 's/go version //')
    # GO_VERSION=$(go version | head -n 1 | cut -d ' ' -f 3)
    log "go version: $GO_VERSION"

    # Run the tests using go test
    log "Unit-testing gosearch"
    cd "$GOSEARCH_PATH"

    log "go test --cover ./..."
    go test --cover ./...

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("gosearch")
    fi

    cd -
}

unittest_groovysearch () {
    echo
    hdr "unittest_groovysearch"
    log "Not currently implemented"
}

unittest_hssearch () {
    echo
    hdr "unittest_hssearch"

    # if ghc is installed, display version
    if [ -n "$(which ghc)" ]
    then
        GHC_VERSION=$(ghc --version)
        log "ghc version: $GHC_VERSION"
    fi

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        FAILED_BUILDS+=("hssearch")
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"

    cd "$HSFIND_PATH"

    # test with stack
    log "Unit-testing hssearch"
    log "stack test"
    stack test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("hssearch")
    fi

    cd -
}

unittest_javasearch () {
    echo
    hdr "unittest_javasearch"

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

unittest_jssearch () {
    echo
    hdr "unittest_jssearch"

    # if node is installed, display version
    if [ -n "$(which node)" ]
    then
        NODE_VERSION=$(node --version)
        log "node version: $NODE_VERSION"
    fi

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("jssearch")
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    cd "$JSSEARCH_PATH"

    # run tests via npm
    log "Unit-testing jssearch"
    log "npm test"
    npm test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("jssearch")
    fi

    cd -
}

unittest_ktsearch () {
    echo
    hdr "unittest_ktsearch"

    GRADLE=
    # check for gradle wrapper
    if [ -f "gradlew" ]
    then
        GRADLE="./gradlew"
    elif [ -n "$(which gradle)" ]
    then
        GRADLE="gradle"
    else
        log_error "You need to install gradle"
        FAILED_BUILDS+=("ktsearch")
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    cd "$KTSEARCH_PATH"

    # run tests via gradle
    log "Unit-testing ktsearch"
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("ktsearch")
    fi

    cd -
}

unittest_objcsearch () {
    echo
    hdr "unittest_objcsearch"

    # TODO: copy resource files locally?
    # ensure xcode is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("objcsearch")
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    cd "$OBJCSEARCH_PATH"

    log "Unit-testing objcsearch"
    log "swift test"
    swift test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("objcsearch")
    fi

    cd -
}

# unittest_mlsearch () {
#     echo
#     hdr "unittest_mlsearch"
#
#     cd "$MLSEARCH_PATH"
#
#     log "Unit-testing mlsearch"
#     ./unittest.sh
#
#     cd -
# }

unittest_plsearch () {
    echo
    hdr "unittest_plsearch"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        FAILED_BUILDS+=("plsearch")
        return
    fi

    PERL_VERSION="$(perl -e 'print $^V' | grep '^v5')"
    if [ -z $PERL_VERSION ]
    then
        log_error "A 5.x version of perl is required"
        FAILED_BUILDS+=("plsearch")
        return
    fi

    log "perl version: $PERL_VERSION"

    TESTS_PATH="$PLSEARCH_PATH/t"

    # run tests using Test::Simple
    log "Unit-testing plsearch"
    FILES=$(find "$TESTS_PATH" -name "*_test.pl" | sort)
    for f in ${FILES[*]}
    do
        log "perl $f"
        IFS='' perl "$f" | tail -n +2 |
        while read line
        do
            echo "$line"
            if [[ ! "$line" =~ ^ok[[:space:]][0-9]+.+$ ]]
            then
                log_error "Tests failed"
                FAILED_BUILDS+=("plsearch")
                return
            fi
        done
    done
}

unittest_phpsearch () {
    echo
    hdr "unittest_phpsearch"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        FAILED_BUILDS+=("phpsearch")
        return
    fi

    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        FAILED_BUILDS+=("phpsearch")
        return
    fi
    log "php version: $PHP_VERSION"

    # if composer is installed, display version
    if [ -n "$(which composer)" ]
    then
        COMPOSER_VERSION=$(composer --version 2>&1 | grep '^Composer')
        log "composer version: $COMPOSER_VERSION"
    fi

    TESTS_PATH="$PHPSEARCH_PATH/tests"
    PHPUNIT="$PHPSEARCH_PATH/vendor/bin/phpunit"

    if [ ! -f "$PHPUNIT" ]
    then
        log_error "You need to install phpunit first"
        FAILED_BUILDS+=("phpsearch")
        return
    fi

    # run tests with phpunit
    log "Unit-testing phpsearch"
    log "$PHPUNIT $TESTS_PATH"
    "$PHPUNIT" "$TESTS_PATH"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("phpsearch")
    fi
}

unittest_ps1search () {
    echo
    hdr "unittest_ps1search"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        FAILED_BUILDS+=("ps1search")
        return
    fi

    POWERSHELL_VERSION=$(pwsh -v)
    log "powershell version: $POWERSHELL_VERSION"

    TESTS_SCRIPT="$PS1SEARCH_PATH/ps1search.tests.ps1"
    if [ ! -f "$TESTS_SCRIPT" ]
    then
        log_error "Test script not found: $TESTS_SCRIPT"
        FAILED_BUILDS+=("ps1search")
        return
    fi

    # run tests with powershell
    log "Unit-testing ps1search"
    log "pwsh $TESTS_SCRIPT"
    pwsh "$TESTS_SCRIPT"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("ps1search")
    fi
}

unittest_pysearch () {
    echo
    hdr "unittest_pysearch"

    TESTS_PATH="$PYSEARCH_PATH/tests"
    VENV_PATH="$PYSEARCH_PATH/venv"
    PYTHON="$VENV_PATH/bin/python"
    export PYTHONPATH="$PYTHON_PATH"

    if [ ! -d "$VENV_PATH" ]
    then
        log_error "venv path not found, you probably need to run the python build (./build.sh python)"
        FAILED_BUILDS+=("pysearch")
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

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("pysearch")
    fi

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    cd -
}

unittest_rbsearch () {
    echo
    hdr "unittest_rbsearch"

    # ensure ruby3.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        FAILED_BUILDS+=("rbsearch")
        return
    fi

    RUBY_VERSION="$(ruby -v 2>&1 | grep '^ruby 3')"
    if [ -z "$RUBY_VERSION" ]
    then
        log_error "A version of ruby >= 3.x is required"
        FAILED_BUILDS+=("rbsearch")
        return
    fi

    log "ruby version: $RUBY_VERSION"

    # ensure bundler is installed
    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        FAILED_BUILDS+=("rbsearch")
        return
    fi

    # ensure rake is installed
    if [ -z "$(which rake)" ]
    then
        log_error "You need to install rake"
        FAILED_BUILDS+=("rbsearch")
        return
    fi

    cd "$RBSEARCH_PATH"

    # Run all tests via rake
    log "Unit-testing rbsearch"
    log "bundle exec rake test"
    bundle exec rake test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("rbsearch")
    fi

    cd -
}

unittest_rssearch () {
    echo
    hdr "unittest_rssearch"

    # if rust is installed, display version
    if [ -n "$(which rustc)" ]
    then
        RUST_VERSION=$(rustc --version)
        log "rustc version: $RUST_VERSION"
    fi

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        FAILED_BUILDS+=("rssearch")
        return
    fi

    CARGO_VERSION=$(cargo --version)
    log "cargo version: $CARGO_VERSION"

    cd "$RSSEARCH_PATH"

    # Run cargo test
    log "Unit-testing rssearch"
    log "cargo test --package rssearch --bin rssearch"
    cargo test --package rssearch --bin rssearch

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("rssearch")
    fi

    cd -
}

unittest_scalasearch () {
    echo
    hdr "unittest_scalasearch"

    # if scala is installed, display version
    if [ -n "$(which scala)" ]
    then
        SCALA_VERSION=$(scala -version 2>&1 | tail -n 1)
        log "$SCALA_VERSION"
    fi

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install sbt"
        FAILED_BUILDS+=("scalasearch")
        return
    fi

    SBT_OUTPUT=$(sbt --version)

    SBT_PROJECT_VERSION=$(echo "$SBT_OUTPUT" | grep 'project')
    log "$SBT_PROJECT_VERSION"

    SBT_SCRIPT_VERSION=$(echo "$SBT_OUTPUT" | grep 'script')
    log "$SBT_SCRIPT_VERSION"

    JDK_VERSION=$(java -version  2>&1 | head -n 1)
    log "JDK version: $JDK_VERSION"

    cd "$SCALASEARCH_PATH"

    # run tests via sbt
    log "Unit-testing scalasearch"
    log "sbt test"
    sbt test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("scalasearch")
    fi

    cd -
}

unittest_swiftsearch () {
    echo
    hdr "unittest_swiftsearch"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("swiftsearch")
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    cd "$SWIFTSEARCH_PATH"

    # run tests
    log "Unit-testing swiftsearch"
    log "swift test"
    swift test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("swiftsearch")
    fi

    cd -
}

unittest_tssearch () {
    echo
    hdr "unittest_tssearch"

    # if node is installed, display version
    if [ -n "$(which node)" ]
    then
        NODE_VERSION=$(node --version)
        log "node version: $NODE_VERSION"
    fi

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("tssearch")
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    cd "$TSSEARCH_PATH"

    # run tests
    log "Unit-testing tssearch"
    log "npm test"
    npm test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("tssearch")
    fi

    cd -
}

unittest_all () {
    hdr "unittest_all"

    # unittest_bashsearch

    # unittest_csearch

    unittest_cljsearch

    unittest_cppsearch

    unittest_cssearch

    unittest_dartsearch

    unittest_exsearch

    unittest_fssearch

    unittest_gosearch

    # unittest_groovysearch

    unittest_hssearch

    unittest_javasearch

    unittest_jssearch

    unittest_ktsearch

    unittest_objcsearch

    # unittest_mlsearch

    unittest_plsearch

    unittest_phpsearch

    unittest_ps1search

    unittest_pysearch

    unittest_rbsearch

    unittest_rssearch

    unittest_scalasearch

    unittest_swiftsearch

    unittest_tssearch
}


########################################
# Unit-testing main
########################################
echo
hdr "xsearch unittest script"
log "user: $USER"
log "host: $HOSTNAME"
log "os: $(uname -o)"

# Get the current git branch and commit
# GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

HELP=
TEST_ALL=
TARGET_LANGS=()

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
        --all | all)
            TEST_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "TEST_ALL: $TEST_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$TEST_ALL" ]
then
    unittest_all
    print_failed_builds
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        # bash)
        #     unittest_bashsearch
        #     ;;
        # c)
        #     unittest_csearch
        #     ;;
        clj | clojure)
            unittest_cljsearch
            ;;
        cpp)
            unittest_cppsearch
            ;;
        cs | csharp)
            unittest_cssearch
            ;;
        dart)
            unittest_dartsearch
            ;;
        elixir | ex)
            unittest_exsearch
            ;;
        fs | fsharp)
            unittest_fssearch
            ;;
        go)
            unittest_gosearch
            ;;
        # groovy)
        #     unittest_groovysearch
        #     ;;
        haskell | hs)
            unittest_hssearch
            ;;
        java)
            unittest_javasearch
            ;;
        javascript | js)
            unittest_jssearch
            ;;
        kotlin | kt)
            unittest_ktsearch
            ;;
        objc)
            unittest_objcsearch
            ;;
        # ocaml | ml)
        #     unittest_mlsearch
        #     ;;
        perl | pl)
            unittest_plsearch
            ;;
        php)
            unittest_phpsearch
            ;;
        ps1 | powershell | pwsh)
            unittest_ps1search
            ;;
        py | python)
            unittest_pysearch
            ;;
        rb | ruby)
            unittest_rbsearch
            ;;
        rs | rust)
            unittest_rssearch
            ;;
        scala)
            unittest_scalasearch
            ;;
        swift)
            unittest_swiftsearch
            ;;
        ts | typescript)
            unittest_tssearch
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_failed_builds
