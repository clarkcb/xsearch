#!/bin/bash
################################################################################
#
# lint.sh
#
# Run static code analysis tools on xsearch language versions
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
source "$XFIND_SCRIPT_DIR/lint_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: lint.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Lint Functions
########################################

lint_xsearch_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="lint_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XSEARCH_PATH" "$version_name"
    else
        log_error "lint function not found: $function_name"
        LINT_LASTEXITCODE=1
    fi

    # log "LINT_LASTEXITCODE: $LINT_LASTEXITCODE"
    if [ "$LINT_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name lint succeeded"
        SUCCESSFUL_LINTS+=($version_name)
    else
        log_error "$version_name lint failed"
        FAILED_LINTS+=($version_name)
    fi
}

lint_bashsearch () {
    echo
    hdr "lint_bashsearch"

    lint_xsearch_version "bash" "bashsearch"
}

lint_csearch () {
    echo
    hdr "lint_csearch"

    lint_xsearch_version "c" "csearch"
}

lint_cljsearch () {
    echo
    hdr "lint_cljsearch"

    lint_xsearch_version "clojure" "cljsearch"
}

lint_cppsearch () {
    echo
    hdr "lint_cppsearch"

    lint_xsearch_version "cpp" "cppsearch"
}

lint_cssearch () {
    echo
    hdr "lint_cssearch"

    lint_xsearch_version "csharp" "cssearch"
}

lint_dartsearch () {
    echo
    hdr "lint_dartsearch"

    lint_xsearch_version "dart" "dartsearch"
}

lint_exsearch () {
    echo
    hdr "lint_exsearch"

    lint_xsearch_version "elixir" "exsearch"
}

lint_fssearch () {
    echo
    hdr "lint_fssearch"

    lint_xsearch_version "fsharp" "fssearch"
}

lint_gosearch () {
    echo
    hdr "lint_gosearch"

    lint_xsearch_version "go" "gosearch"
}

lint_groovysearch () {
    echo
    hdr "lint_groovysearch"

    lint_xsearch_version "groovy" "groovysearch"
}

lint_hssearch () {
    echo
    hdr "lint_hssearch"

    lint_xsearch_version "haskell" "hssearch"
}

lint_javasearch () {
    echo
    hdr "lint_javasearch"

    lint_xsearch_version "java" "javasearch"
}

lint_jssearch () {
    echo
    hdr "lint_jssearch"

    lint_xsearch_version "javascript" "jssearch"
}

lint_ktsearch () {
    echo
    hdr "lint_ktsearch"

    lint_xsearch_version "kotlin" "ktsearch"
}

lint_mlsearch () {
    echo
    hdr "lint_mlsearch"

    # TODO: probably want to delete the _build directory
}

lint_objcsearch () {
    echo
    hdr "lint_objcsearch"

    lint_xsearch_version "objc" "objcsearch"
}

lint_phpsearch () {
    echo
    hdr "lint_phpsearch"

    lint_xsearch_version "php" "phpsearch"
}

lint_plsearch () {
    echo
    hdr "lint_plsearch"

    lint_xsearch_version "perl" "plsearch"
}

lint_ps1search () {
    echo
    hdr "lint_ps1search"
    log "Nothing to do for powershell"

    lint_xsearch_version "powershell" "ps1search"
}

lint_pysearch () {
    echo
    hdr "lint_pysearch"

    lint_xsearch_version "python" "pysearch"
}

lint_rbsearch () {
    echo
    hdr "lint_rbsearch"

    lint_xsearch_version "ruby" "rbsearch"
}

lint_rssearch () {
    echo
    hdr "lint_rssearch"

    lint_xsearch_version "rust" "rssearch"
}

lint_scalasearch () {
    echo
    hdr "lint_scalasearch"

    lint_xsearch_version "scala" "scalasearch"
}

lint_swiftsearch () {
    echo
    hdr "lint_swiftsearch"

    lint_xsearch_version "swift" "swiftsearch"
}

lint_tssearch () {
    echo
    hdr "lint_tssearch"

    lint_xsearch_version "typescript" "tssearch"
}

lint_linux () {
    hdr "lint_linux"

    # lint_bashsearch

    # lint_csearch

    # lint_cljsearch

    # lint_cppsearch

    lint_cssearch

    # lint_dartsearch

    lint_exsearch

    lint_fssearch

    lint_gosearch

    # lint_groovysearch

    # lint_hssearch

    lint_javasearch

    lint_jssearch

    lint_ktsearch

    # lint_objcsearch

    # lint_mlsearch

    lint_phpsearch

    lint_plsearch

    lint_pysearch

    lint_rbsearch

    lint_rssearch

    # lint_scalasearch

    lint_swiftsearch

    lint_tssearch
}

lint_all () {
    hdr "lint_all"

    # lint_bashsearch

    # lint_csearch

    lint_cljsearch

    lint_cppsearch

    lint_cssearch

    lint_dartsearch

    lint_exsearch

    lint_fssearch

    lint_gosearch

    # lint_groovysearch

    lint_hssearch

    lint_javasearch

    lint_jssearch

    lint_ktsearch

    lint_objcsearch

    # lint_mlsearch

    lint_plsearch

    lint_phpsearch

    lint_ps1search

    lint_pysearch

    lint_rbsearch

    lint_rssearch

    lint_scalasearch

    lint_swiftsearch

    lint_tssearch
}


########################################
# Lint Main
########################################
echo
hdr "xsearch lint script"
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
LINT_ALL=
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
            LINT_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "LINT_ALL: $LINT_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$LINT_ALL" ]
then
    lint_all
    print_lint_results
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
            lint_linux
            ;;
        # bash)
        #     lint_bashsearch
        #     ;;
        # c)
        #     lint_csearch
        #     ;;
        clj | clojure)
            lint_cljsearch
            ;;
        cpp)
            lint_cppsearch
            ;;
        cs | csharp)
            lint_cssearch
            ;;
        dart)
            lint_dartsearch
            ;;
        elixir | ex)
            lint_exsearch
            ;;
        fs | fsharp)
            lint_fssearch
            ;;
        go)
            lint_gosearch
            ;;
        # groovy)
        #     lint_groovysearch
        #     ;;
        haskell | hs)
            lint_hssearch
            ;;
        java)
            lint_javasearch
            ;;
        javascript | js)
            lint_jssearch
            ;;
        kotlin | kt)
            lint_ktsearch
            ;;
        objc)
            lint_objcsearch
            ;;
        # ocaml | ml)
        #     lint_mlsearch
        #     ;;
        perl | pl)
            lint_plsearch
            ;;
        php)
            lint_phpsearch
            ;;
        ps1 | powershell)
            lint_ps1search
            ;;
        py | python)
            lint_pysearch
            ;;
        rb | ruby)
            lint_rbsearch
            ;;
        rs | rust)
            lint_rssearch
            ;;
        scala)
            lint_scalasearch
            ;;
        swift)
            lint_swiftsearch
            ;;
        ts | typescript)
            lint_tssearch
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_lint_results
