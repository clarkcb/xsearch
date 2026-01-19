#!/bin/bash
################################################################################
#
# clean.sh
#
# Clean xsearch language versions
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

# Load the generic clean functions from xfind
source "$XFIND_SCRIPT_DIR/clean_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: clean.sh [-h|--help] [--lock] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Clean Functions
########################################

clean_xsearch_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="clean_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XSEARCH_PATH" "$version_name"
    else
        log_error "clean function not found: $function_name"
        CLEAN_LASTEXITCODE=1
    fi

    # log "CLEAN_LASTEXITCODE: $CLEAN_LASTEXITCODE"
    if [ "$CLEAN_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name clean succeeded"
        SUCCESSFUL_CLEANS+=($version_name)
    else
        log_error "$version_name clean failed"
        FAILED_CLEANS+=($version_name)
    fi
}

clean_bashsearch () {
    echo
    hdr "clean_bashsearch"
    log "Nothing to do for bash"
}

clean_csearch () {
    echo
    hdr "clean_csearch"

    clean_xsearch_version "c" "csearch"
}

clean_cljsearch () {
    echo
    hdr "clean_cljsearch"

    clean_xsearch_version "clojure" "cljsearch"
}

clean_cppsearch () {
    echo
    hdr "clean_cppsearch"

    clean_xsearch_version "cpp" "cppsearch"
}

clean_cssearch () {
    echo
    hdr "clean_cssearch"

    clean_xsearch_version "csharp" "cssearch"
}

clean_dartsearch () {
    echo
    hdr "clean_dartsearch"

    clean_xsearch_version "dart" "dartsearch"
}

clean_exsearch () {
    echo
    hdr "clean_exsearch"

    clean_xsearch_version "elixir" "exsearch"
}

clean_fssearch () {
    echo
    hdr "clean_fssearch"

    clean_xsearch_version "fsharp" "fssearch"
}

clean_gosearch () {
    echo
    hdr "clean_gosearch"

    clean_xsearch_version "go" "gosearch"
}

clean_groovysearch () {
    echo
    hdr "clean_groovysearch"

    clean_xsearch_version "groovy" "groovysearch"
}

clean_hssearch () {
    echo
    hdr "clean_hssearch"

    clean_xsearch_version "haskell" "hssearch"
}

clean_javasearch () {
    echo
    hdr "clean_javasearch"

    clean_xsearch_version "java" "javasearch"
}

clean_jssearch () {
    echo
    hdr "clean_jssearch"

    clean_xsearch_version "javascript" "jssearch"
}

clean_ktsearch () {
    echo
    hdr "clean_ktsearch"

    clean_xsearch_version "kotlin" "ktsearch"
}

clean_mlsearch () {
    echo
    hdr "clean_mlsearch"

    clean_xsearch_version "ocaml" "mlsearch"
}

clean_objcsearch () {
    echo
    hdr "clean_objcsearch"

    clean_xsearch_version "objc" "objcsearch"
}

clean_plsearch () {
    echo
    hdr "clean_plsearch"

    clean_xsearch_version "perl" "plsearch"
}

clean_phpsearch () {
    echo
    hdr "clean_phpsearch"

    clean_xsearch_version "php" "phpsearch"
}

clean_ps1search () {
    echo
    hdr "clean_ps1search"

    clean_xsearch_version "powershell" "ps1search"
}

clean_pysearch () {
    echo
    hdr "clean_pysearch"

    clean_xsearch_version "python" "pysearch"
}

clean_rbsearch () {
    echo
    hdr "clean_rbsearch"

    clean_xsearch_version "ruby" "rbsearch"
}

clean_rssearch () {
    echo
    hdr "clean_rssearch"

    clean_xsearch_version "rust" "rssearch"
}

clean_scalasearch () {
    echo
    hdr "clean_scalasearch"

    clean_xsearch_version "scala" "scalasearch"
}

clean_swiftsearch () {
    echo
    hdr "clean_swiftsearch"

    clean_xsearch_version "swift" "swiftsearch"
}

clean_tssearch () {
    echo
    hdr "clean_tssearch"

    clean_xsearch_version "typescript" "tssearch"
}

clean_linux () {
    hdr "clean_linux"

    # clean_bashsearch

    # clean_csearch

    # clean_cljsearch

    # clean_cppsearch

    clean_cssearch

    # clean_dartsearch

    clean_exsearch

    clean_fssearch

    clean_gosearch

    # clean_groovysearch

    # clean_hssearch

    clean_javasearch

    clean_jssearch

    clean_ktsearch

    # clean_objcsearch

    # clean_mlsearch

    clean_phpsearch

    clean_plsearch

    clean_pysearch

    clean_rbsearch

    clean_rssearch

    # clean_scalasearch

    clean_swiftsearch

    clean_tssearch
}

clean_all () {
    hdr "clean_all"

    # clean_bashsearch

    # clean_csearch

    clean_cljsearch

    clean_cppsearch

    clean_cssearch

    clean_dartsearch

    clean_exsearch

    clean_fssearch

    clean_gosearch

    # clean_groovysearch

    clean_hssearch

    clean_javasearch

    clean_jssearch

    clean_ktsearch

    clean_objcsearch

    # clean_mlsearch

    clean_plsearch

    clean_phpsearch

    clean_ps1search

    clean_pysearch

    clean_rbsearch

    clean_rssearch

    clean_scalasearch

    clean_swiftsearch

    clean_tssearch
}


########################################
# Clean Main
########################################
echo
hdr "xsearch clean script"
log "user: $USER"
log "host: $HOSTNAME"
log "os: $(uname -o)"

# Get the current git branch and commit
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

CLEAN_ALL=
HELP=
LOCKFILE=
TARGET_LANGS=()

if [ $# == 0 ]
then
    HELP=yes
fi

while [ -n "$1" ]
do
    case "$1" in
        --all | all)
            CLEAN_ALL=yes
            ;;
        -h | --help)
            HELP=yes
            ;;
        --lock)
            LOCKFILE=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "CLEAN_ALL: $CLEAN_ALL"
log "HELP: $HELP"
log "LOCKFILE: $LOCKFILE"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$CLEAN_ALL" ]
then
    clean_all
    print_clean_results
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
            clean_linux
            ;;
        # bash)
        #     clean_bashsearch
        #     ;;
        # c)
        #     clean_csearch
        #     ;;
        clj | clojure)
            clean_cljsearch
            ;;
        cpp)
            clean_cppsearch
            ;;
        cs | csharp)
            clean_cssearch
            ;;
        dart)
            clean_dartsearch
            ;;
        elixir | ex)
            clean_exsearch
            ;;
        fs | fsharp)
            clean_fssearch
            ;;
        go)
            clean_gosearch
            ;;
        # groovy)
        #     clean_groovysearch
        #     ;;
        haskell | hs)
            clean_hssearch
            ;;
        java)
            clean_javasearch
            ;;
        javascript | js)
            clean_jssearch
            ;;
        kotlin | kt)
            clean_ktsearch
            ;;
        objc)
            clean_objcsearch
            ;;
        # ocaml | ml)
        #     clean_mlsearch
        #     ;;
        perl | pl)
            clean_plsearch
            ;;
        php)
            clean_phpsearch
            ;;
        ps1 | powershell)
            clean_ps1search
            ;;
        py | python)
            clean_pysearch
            ;;
        rb | ruby)
            clean_rbsearch
            ;;
        rs | rust)
            clean_rssearch
            ;;
        scala)
            clean_scalasearch
            ;;
        swift)
            clean_swiftsearch
            ;;
        ts | typescript)
            clean_tssearch
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_clean_results
