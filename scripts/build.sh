#!/bin/bash
################################################################################
#
# build.sh
#
# Build xsearch language versions
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

# Load the generic build functions from xfind
source "$XFIND_SCRIPT_DIR/build_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: build.sh [-h|--help] [--debug] [--release] [--venv] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Build Functions
########################################

build_xsearch_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="build_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XSEARCH_PATH" "$version_name"
    else
        log_error "build function not found: $function_name"
        BUILD_LASTEXITCODE=1
    fi

    # log "BUILD_LASTEXITCODE: $BUILD_LASTEXITCODE"
    if [ "$BUILD_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name build succeeded"
        SUCCESSFUL_BUILDS+=($version_name)
    else
        log_error "$version_name build failed"
        FAILED_BUILDS+=($version_name)
    fi
}

build_bashsearch () {
    echo
    hdr "build_bashsearch"

    build_xsearch_version "bash" "bashsearch"
}

build_csearch () {
    echo
    hdr "build_csearch"

    build_xsearch_version "c" "csearch"
}

build_cljsearch () {
    echo
    hdr "build_cljsearch"

    build_xsearch_version "clojure" "cljsearch"
}

build_cppsearch () {
    echo
    hdr "build_cppsearch"

    build_xsearch_version "cpp" "cppsearch"
}

build_cssearch () {
    echo
    hdr "build_cssearch"

    build_xsearch_version "csharp" "cssearch"
}

build_dartsearch () {
    echo
    hdr "build_dartsearch"

    build_xsearch_version "dart" "dartsearch"
}

build_exsearch () {
    echo
    hdr "build_exsearch"

    build_xsearch_version "elixir" "exsearch"
}

build_fssearch () {
    echo
    hdr "build_fssearch"

    build_xsearch_version "fsharp" "fssearch"
}

build_gosearch () {
    echo
    hdr "build_gosearch"

    build_xsearch_version "go" "gosearch"
}

build_groovysearch () {
    echo
    hdr "build_groovysearch"

    build_xsearch_version "groovy" "groovysearch"
}

build_hssearch () {
    echo
    hdr "build_hssearch"

    build_xsearch_version "haskell" "hssearch"
}

build_javasearch () {
    echo
    hdr "build_javasearch"

    build_xsearch_version "java" "javasearch"
}

build_jssearch () {
    echo
    hdr "build_jssearch"

    build_xsearch_version "javascript" "jssearch"
}

build_ktsearch () {
    echo
    hdr "build_ktsearch"

    build_xsearch_version "kotlin" "ktsearch"
}

build_mlsearch () {
    echo
    hdr "build_mlsearch"

    build_xsearch_version "ocaml" "mlsearch"
}

build_objcsearch () {
    echo
    hdr "build_objcsearch"

    build_xsearch_version "objc" "objcsearch"
}

build_plsearch () {
    echo
    hdr "build_plsearch"

    build_xsearch_version "perl" "plsearch"
}

build_phpsearch () {
    echo
    hdr "build_phpsearch"

    build_xsearch_version "php" "phpsearch"
}

build_ps1search () {
    echo
    hdr "build_ps1search"

    build_xsearch_version "powershell" "ps1search"
}

build_pysearch () {
    echo
    hdr "build_pysearch"

    build_xsearch_version "python" "pysearch"
}

build_rbsearch () {
    echo
    hdr "build_rbsearch"

    build_xsearch_version "ruby" "rbsearch"
}

build_rssearch () {
    echo
    hdr "build_rssearch"

    build_xsearch_version "rust" "rssearch"
}

build_scalasearch () {
    echo
    hdr "build_scalasearch"

    build_xsearch_version "scala" "scalasearch"
}

build_swiftsearch () {
    echo
    hdr "build_swiftsearch"

    build_xsearch_version "swift" "swiftsearch"
}

build_tssearch () {
    echo
    hdr "build_tssearch"

    build_xsearch_version "typescript" "tssearch"
}

# build_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - build_cljsearch   - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - build_cppsearch   - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - build_gosearch    - go is known for having very fast builds, and it's true, the only builds that are faster here
#                     are the ones that do nothing except copy over resources files (e.g. perl)
# - build_hssearch    - having some dependency issues that need to work through to get it buildling again
# - build_jssearch    - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                     in an external terminal fixes the problem
# - build_ktsearch    - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                     time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                     in a while
# - build_objcsearch  - not sure if it's even possible to build this on linux, but excluding for now
# - build_mlsearch    - had a number of different issues trying to get this version building again, finally
#                     gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - build_rssearch      - the first time this build is run it will pretty time-consuming, particularly for release
#                     target, but intermittent builds should be pretty fast
# - build_scalasearch - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - build_tssearch    - this build has the same problem as build_jssearch; run the build in an external terminal
build_linux () {
    hdr "build_linux"

    # time build_bashsearch

    # time build_csearch

    # time build_cljsearch

    # time build_cppsearch

    time build_cssearch

    time build_dartsearch

    time build_fssearch

    time build_gosearch

    # time build_groovysearch

    time build_javasearch

    time build_jssearch

    # time build_ktsearch

    time build_plsearch

    time build_phpsearch

    time build_pysearch

    time build_rbsearch

    time build_rssearch

    # time build_scalasearch

    time build_swiftsearch

    time build_tssearch
}

build_all () {
    hdr "build_all"

    # time build_bashsearch

    # time build_csearch

    time build_cljsearch

    time build_cppsearch

    time build_cssearch

    time build_dartsearch

    time build_exsearch

    time build_fssearch

    time build_gosearch

    # time build_groovysearch

    time build_hssearch

    time build_javasearch

    time build_jssearch

    time build_ktsearch

    time build_objcsearch

    # time build_mlsearch

    time build_plsearch

    time build_phpsearch

    time build_ps1search

    time build_pysearch

    time build_rbsearch

    time build_rssearch

    time build_scalasearch

    time build_swiftsearch

    time build_tssearch
}


########################################
# Build Main
########################################
echo
hdr "xsearch build script"
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
BUILD_ALL=
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
            BUILD_ALL=yes
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
log "BUILD_ALL: $BUILD_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$BUILD_ALL" ]
then
    build_all
    print_build_results
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
            build_linux
            ;;
        # bash)
        #     time build_bashsearch
        #     ;;
        # c)
        #     time build_csearch
        #     ;;
        clj | clojure)
            time build_cljsearch
            ;;
        cpp)
            time build_cppsearch
            ;;
        cs | csharp)
            time build_cssearch
            ;;
        dart)
            time build_dartsearch
            ;;
        ex | elixir)
            time build_exsearch
            ;;
        fs | fsharp)
            time build_fssearch
            ;;
        go)
            time build_gosearch
            ;;
        # groovy)
        #     time build_groovysearch
        #     ;;
        haskell | hs)
            time build_hssearch
            ;;
        java)
            time build_javasearch
            ;;
        javascript | js)
            time build_jssearch
            ;;
        kotlin | kt)
            time build_ktsearch
            ;;
        objc)
            time build_objcsearch
            ;;
        # ocaml | ml)
        #     time build_mlsearch
        #     ;;
        perl | pl)
            time build_plsearch
            ;;
        php)
            time build_phpsearch
            ;;
        ps1 | powershell | pwsh)
            time build_ps1search
            ;;
        py | python)
            time build_pysearch
            ;;
        rb | ruby)
            time build_rbsearch
            ;;
        rs | rust)
            time build_rssearch
            ;;
        scala)
            time build_scalasearch
            ;;
        swift)
            time build_swiftsearch
            ;;
        ts | typescript)
            time build_tssearch
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_build_results
