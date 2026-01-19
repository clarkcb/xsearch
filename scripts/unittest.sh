#!/bin/bash
################################################################################
#
# unittest.sh
#
# Run unit tests of xsearch language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

source "$DIR/config.sh"
# source "$DIR/common.sh"

if [ -z "$XFIND_PATH" ]
then
    log_error "XFIND_PATH not defined"
    exit 1
fi

if [ ! -d "$XFIND_PATH" ]
then
    log_error "XFIND_PATH directory not found"
    exit 1
fi

XFIND_SCRIPT_DIR="$XFIND_PATH/scripts"

if [ ! -d "$XFIND_PATH" ]
then
    log_error "XFIND_PATH/scripts directory not found"
    exit 1
fi

# Load the generic unittest functions from xfind
source "$XFIND_SCRIPT_DIR/unittest_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Unit Test Functions
########################################

unittest_xsearch_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="unittest_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XSEARCH_PATH" "$version_name"
    else
        log_error "unittest function not found: $function_name"
        UNITTEST_LASTEXITCODE=1
    fi

    # log "UNITTEST_LASTEXITCODE: $UNITTEST_LASTEXITCODE"
    if [ "$UNITTEST_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name tests succeeded"
        SUCCESSFUL_TESTS+=($version_name)
    else
        log_error "$version_name tests failed"
        FAILED_TESTS+=($version_name)
    fi
}

unittest_bashsearch () {
    echo
    hdr "unittest_bashsearch"

    unittest_xsearch_version "bash" "bashsearch"
}

unittest_csearch () {
    echo
    hdr "unittest_csearch"

    unittest_xsearch_version "c" "csearch"
}

unittest_cljsearch () {
    echo
    hdr "unittest_cljsearch"

    unittest_xsearch_version "clojure" "cljsearch"
}

unittest_cppsearch () {
    echo
    hdr "unittest_cppsearch"

    unittest_xsearch_version "cpp" "cppsearch"
}

unittest_cssearch () {
    echo
    hdr "unittest_cssearch"

    unittest_xsearch_version "csharp" "cssearch"
}

unittest_dartsearch () {
    echo
    hdr "unittest_dartsearch"

    unittest_xsearch_version "dart" "dartsearch"
}

unittest_exsearch () {
    echo
    hdr "unittest_exsearch"

    unittest_xsearch_version "elixir" "exsearch"
}

unittest_fssearch () {
    echo
    hdr "unittest_fssearch"

    unittest_xsearch_version "fsharp" "fssearch"
}

unittest_gosearch () {
    echo
    hdr "unittest_gosearch"

    unittest_xsearch_version "go" "gosearch"
}

unittest_groovysearch () {
    echo
    hdr "unittest_groovysearch"

    unittest_xsearch_version "groovy" "groovysearch"
}

unittest_hssearch () {
    echo
    hdr "unittest_hssearch"

    unittest_xsearch_version "haskell" "hssearch"
}

unittest_javasearch () {
    echo
    hdr "unittest_javasearch"

    unittest_xsearch_version "java" "javasearch"
}

unittest_jssearch () {
    echo
    hdr "unittest_jssearch"

    unittest_xsearch_version "javascript" "jssearch"
}

unittest_ktsearch () {
    echo
    hdr "unittest_ktsearch"

    unittest_xsearch_version "kotlin" "ktsearch"
}

unittest_mlsearch () {
    echo
    hdr "unittest_mlsearch"

    unittest_xsearch_version "ocaml" "mlsearch"
}

unittest_objcsearch () {
    echo
    hdr "unittest_objcsearch"

    unittest_xsearch_version "objc" "objcsearch"
}

unittest_plsearch () {
    echo
    hdr "unittest_plsearch"

    unittest_xsearch_version "perl" "plsearch"
}

unittest_phpsearch () {
    echo
    hdr "unittest_phpsearch"

    unittest_xsearch_version "php" "phpsearch"
}

unittest_ps1search () {
    echo
    hdr "unittest_ps1search"

    unittest_xsearch_version "powershell" "ps1search"
}

unittest_pysearch () {
    echo
    hdr "unittest_pysearch"

    unittest_xsearch_version "python" "pysearch"
}

unittest_rbsearch () {
    echo
    hdr "unittest_rbsearch"

    unittest_xsearch_version "ruby" "rbsearch"
}

unittest_rssearch () {
    echo
    hdr "unittest_rssearch"

    unittest_xsearch_version "rust" "rssearch"
}

unittest_scalasearch () {
    echo
    hdr "unittest_scalasearch"

    unittest_xsearch_version "scala" "scalasearch"
}

unittest_swiftsearch () {
    echo
    hdr "unittest_swiftsearch"

    unittest_xsearch_version "swift" "swiftsearch"
}

unittest_tssearch () {
    echo
    hdr "unittest_tssearch"

    unittest_xsearch_version "typescript" "tssearch"
}

# unittest_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - unittest_cljsearch   - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - unittest_cppsearch   - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - unittest_gosearch    - go is known for having very fast builds, and it's true, the only builds that are faster here
#                     are the ones that do nothing except copy over resources files (e.g. perl)
# - unittest_hssearch    - having some dependency issues that need to work through to get it buildling again
# - unittest_jssearch    - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                     in an external terminal fixes the problem
# - unittest_ktsearch    - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                     time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                     in a while
# - unittest_objcsearch  - not sure if it's even possible to build this on linux, but excluding for now
# - unittest_mlsearch    - had a number of different issues trying to get this version building again, finally
#                     gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - unittest_rssearch      - the first time this build is run it will pretty time-consuming, particularly for release
#                     target, but intermittent builds should be pretty fast
# - unittest_scalasearch - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - unittest_tssearch    - this build has the same problem as unittest_jssearch; run the build in an external terminal
unittest_linux () {
    hdr "unittest_linux"

    time unittest_bashsearch

    time unittest_csearch

    # time unittest_cljsearch

    # time unittest_cppsearch

    time unittest_cssearch

    time unittest_dartsearch

    time unittest_fssearch

    time unittest_gosearch

    # time unittest_groovysearch

    time unittest_javasearch

    time unittest_jssearch

    # time unittest_ktsearch

    time unittest_plsearch

    time unittest_phpsearch

    time unittest_pysearch

    time unittest_rbsearch

    time unittest_rssearch

    # time unittest_scalasearch

    time unittest_swiftsearch

    time unittest_tssearch
}

unittest_all () {
    hdr "unittest_all"

    time unittest_bashsearch

    time unittest_csearch

    time unittest_cljsearch

    time unittest_cppsearch

    time unittest_cssearch

    time unittest_dartsearch

    time unittest_exsearch

    time unittest_fssearch

    time unittest_gosearch

    time unittest_groovysearch

    time unittest_hssearch

    time unittest_javasearch

    time unittest_jssearch

    time unittest_ktsearch

    time unittest_objcsearch

    # time unittest_mlsearch

    time unittest_plsearch

    time unittest_phpsearch

    time unittest_ps1search

    time unittest_pysearch

    time unittest_rbsearch

    time unittest_rssearch

    time unittest_scalasearch

    time unittest_swiftsearch

    time unittest_tssearch
}


########################################
# Unit Test Main
########################################
echo
hdr "xsearch unittest script"
log "user: $USER"
log "host: $HOSTNAME"
log "os: $(uname -o)"

# Get the current git branch and commit
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

HELP=
DEBUG=
RELEASE=
VENV=
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
        --debug)
            DEBUG=yes
            ;;
        --release)
            RELEASE=yes
            ;;
        --venv)
            VENV=yes
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

if [ -z "$RELEASE" ]
then
    DEBUG=yes
fi

# log the settings
log "HELP: $HELP"
log "DEBUG: $DEBUG"
log "RELEASE: $RELEASE"
log "VENV: $VENV"
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
    print_test_results
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        linux)
            unittest_linux
            ;;
        # bash)
        #     time unittest_bashsearch
        #     ;;
        # c)
        #     time unittest_csearch
        #     ;;
        clj | clojure)
            time unittest_cljsearch
            ;;
        cpp)
            time unittest_cppsearch
            ;;
        cs | csharp)
            time unittest_cssearch
            ;;
        dart)
            time unittest_dartsearch
            ;;
        ex | elixir)
            time unittest_exsearch
            ;;
        fs | fsharp)
            time unittest_fssearch
            ;;
        go)
            time unittest_gosearch
            ;;
        # groovy)
        #     time unittest_groovysearch
        #     ;;
        haskell | hs)
            time unittest_hssearch
            ;;
        java)
            time unittest_javasearch
            ;;
        javascript | js)
            time unittest_jssearch
            ;;
        kotlin | kt)
            time unittest_ktsearch
            ;;
        objc)
            time unittest_objcsearch
            ;;
        # ocaml | ml)
        #     time unittest_mlsearch
        #     ;;
        perl | pl)
            time unittest_plsearch
            ;;
        php)
            time unittest_phpsearch
            ;;
        ps1 | powershell | pwsh)
            time unittest_ps1search
            ;;
        py | python)
            time unittest_pysearch
            ;;
        rb | ruby)
            time unittest_rbsearch
            ;;
        rs | rust)
            time unittest_rssearch
            ;;
        scala)
            time unittest_scalasearch
            ;;
        swift)
            time unittest_swiftsearch
            ;;
        ts | typescript)
            time unittest_tssearch
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_test_results
