#!/bin/sh
################################################################################
#
# clean.sh
#
# Runs a clean (remove generated files) for each language version
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
    echo -e "\nUsage: clean.sh [-h|--help] {\"all\" | langcode}\n"
    exit
}


########################################
# Clean Functions
########################################

clean_bashsearch () {
    echo
    hdr "clean_bashsearch"
    log "Nothing to do for bash"
}

clean_csearch () {
    echo
    hdr "clean_csearch"

    cd "$CSEARCH_PATH"

    for c in $(find . -name "cmake-build-*" -type d -maxdepth 1)
    do
        log "rm -rf $c"
        rm -rf "$c"
    done

    cd -
}

clean_cljsearch () {
    echo
    hdr "clean_cljsearch"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install lein"
        return
    fi

    cd "$CLJSEARCH_PATH"

    log "lein clean"
    lein clean

    cd -
}

clean_cppsearch () {
    echo
    hdr "clean_cppsearch"

    cd "$CPPSEARCH_PATH"

    for c in $(find . -name "cmake-build-*" -type d -maxdepth 1)
    do
        log "rm -rf $c"
        rm -rf "$c"
    done

    cd -
}

clean_cssearch () {
    echo
    hdr "clean_cssearch"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    cd "$CSSEARCH_PATH"

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    log "dotnet clean -v minimal"
    dotnet clean -v minimal

    for p in $(find "$CSSEARCH_PATH" -name "CsSearch*" -type d -maxdepth 1)
    do
        if [ -d "$p" ]
        then
            log "rm -rf $p/bin"
            rm -rf "$p/bin"

            log "rm -rf $p/obj"
            rm -rf "$p/obj"
        fi
    done

    cd -
}

clean_dartsearch () {
    echo
    hdr "clean_dartsearch"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        echo "You need to install dart"
        return
    fi

    # pub cache repair is apparently the closest thing to clean for dart
    cd "$DARTSEARCH_PATH"

    log "dart pub cache repair"
    dart pub cache repair

    cd -
}

clean_exsearch () {
    echo
    hdr "clean_exsearch"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        return
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        return
    fi

    cd "$EXSEARCH_PATH"

    log "mix clean"
    mix clean

    cd -
}

clean_fssearch () {
    echo
    hdr "clean_fssearch"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    cd "$FSSEARCH_PATH"

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    log "dotnet clean -v minimal"
    dotnet clean -v minimal

    for p in $(find "$FSSEARCH_PATH" -name "FsSearch*" -type d -maxdepth 1)
    do
        if [ -d "$p" ]
        then
            log "rm -rf $p/bin"
            rm -rf "$p/bin"

            log "rm -rf $p/obj"
            rm -rf "$p/obj"
        fi
    done

    cd -
}

clean_gosearch () {
    echo
    hdr "clean_gosearch"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

    cd "$GOSEARCH_PATH"

    log "go clean"
    go clean

    cd -
}

clean_hssearch () {
    echo
    hdr "clean_hssearch"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

    cd "$HSSEARCH_PATH"

    log "stack clean"
    stack clean

    cd -
}

clean_javasearch () {
    echo
    hdr "clean_javasearch"

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        echo "You need to install mvn"
        return
    fi

    log "mvn -f $JAVASEARCH_PATH/pom.xml clean"
    mvn -f $JAVASEARCH_PATH/pom.xml clean
}

clean_jssearch () {
    echo
    hdr "clean_jssearch"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

    cd $JSSEARCH_PATH

    log "npm run clean"
    npm run clean

    cd -
}

clean_ktsearch () {
    echo
    hdr "clean_ktsearch"

    cd "$KTSEARCH_PATH"

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
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    cd -
}

clean_objcsearch () {
    echo
    hdr "clean_objcsearch"

    cd "$OBJCSEARCH_PATH"

    log "swift package clean"
    swift package clean

    cd -
}

clean_mlsearch () {
    echo
    hdr "clean_mlsearch"

    # TODO: probably want to delete the _build directory
}

clean_plsearch () {
    echo
    hdr "clean_plsearch"
    log "Nothing to do for perl"
}

clean_phpsearch () {
    echo
    hdr "clean_phpsearch"
    log "Nothing to do for php"
}

clean_pysearch () {
    echo
    hdr "clean_pysearch"
    log "Nothing to do for python"
}

clean_rbsearch () {
    echo
    hdr "clean_rbsearch"
    log "Nothing to do for ruby"
}

clean_rssearch () {
    echo
    hdr "clean_rssearch"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        echo "You need to install cargo/rust"
        return
    fi

    cd "$RSSEARCH_PATH"

    echo "cargo clean"
    cargo clean

    cd -
}

clean_scalasearch () {
    echo
    hdr "clean_scalasearch"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        echo "You need to install sbt"
        return
    fi

    # TODO: convert to sbt command

    cd "$SCALASEARCH_PATH"

    log "sbt clean"
    sbt clean

    cd -
}

clean_swiftsearch () {
    echo
    hdr "clean_swiftsearch"

    cd "$SWIFTSEARCH_PATH"

    log "swift package clean"
    swift package clean

    cd -
}

clean_tssearch () {
    echo
    hdr "clean_tssearch"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

    cd "$TSSEARCH_PATH"

    log "npm run clean"
    npm run clean

    cd -
}

clean_linux () {
    hdr "clean_linux"

    # clean_csearch

    # clean_cljsearch

    # clean_cppsearch

    clean_cssearch

    # clean_dartsearch

    clean_fssearch

    clean_gosearch

    # clean_hssearch

    clean_javasearch

    clean_jssearch

    clean_ktsearch

    # clean_objcsearch

    # clean_mlsearch

    clean_plsearch

    clean_phpsearch

    clean_pysearch

    clean_rbsearch

    clean_rssearch

    # clean_scalasearch

    clean_swiftsearch

    clean_tssearch
}

clean_all () {
    hdr "clean_all"

    # clean_csearch

    clean_cljsearch

    clean_cppsearch

    clean_cssearch

    clean_dartsearch

    clean_fssearch

    clean_gosearch

    clean_hssearch

    clean_javasearch

    clean_jssearch

    clean_ktsearch

    clean_objcsearch

    clean_mlsearch

    clean_plsearch

    clean_phpsearch

    clean_pysearch

    clean_rbsearch

    clean_rssearch

    clean_scalasearch

    clean_swiftsearch

    clean_tssearch
}


########################################
# Clean Steps
########################################
echo
hdr "xsearch clean script"
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
CLEAN_ALL=
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
            CLEAN_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "CLEAN_ALL: $CLEAN_ALL"
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
        c)
            clean_csearch
            ;;
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
        # ps1 | powershell)
        #     clean_powershell
        #     ;;
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
