#!/bin/bash
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

# Add failed builds to this array and report failed builds at the end
FAILED_BUILDS=()


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: clean.sh [-h|--help] [--lock] {\"all\" | lang [lang...]}\n"
    exit
}

# clean_json_resources
clean_json_resources () {
    local resources_path="$1"
    for f in $(find "$resources_path" -name "*.json" -type f -maxdepth 1)
    do
        log "rm $f"
        rm "$f"
    done
}

# clean_test_resources
clean_test_resources () {
    local resources_path="$1"
    for f in $(find "$resources_path" -name "testFile*.txt" -type f -maxdepth 1)
    do
        log "rm $f"
        rm "$f"
    done
}

print_failed_builds () {
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed cleans: ${FAILED_BUILDS[*]}"
    else
        log "All cleans succeeded"
    fi
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
        log_error "You need to install lein"
        FAILED_BUILDS+=("cljsearch")
        return
    fi

    cd "$CLJSEARCH_PATH"

    log "lein clean"
    lein clean

    clean_json_resources "$CLJSEARCH_PATH/resources"

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
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("cssearch")
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

    clean_json_resources "$CSSEARCH_PATH/CsSearchLib/Resources"

    clean_test_resources "$CSSEARCH_PATH/CsSearchTests/Resources"

    cd -
}

clean_dartsearch () {
    echo
    hdr "clean_dartsearch"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        FAILED_BUILDS+=("dartsearch")
        return
    fi

    cd "$DARTSEARCH_PATH"

    # pub cache repair is apparently the closest thing to clean for dart
    # but unfortunately it's pretty slow
    log "dart pub cache repair"
    dart pub cache repair

    if [ -n "$LOCKFILE" -a -f "pubspec.lock" ]
    then
        log "rm pubspec.lock"
        rm -f pubspec.lock
    fi

    cd -
}

clean_exsearch () {
    echo
    hdr "clean_exsearch"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        FAILED_BUILDS+=("exsearch")
        return
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        FAILED_BUILDS+=("exsearch")
        return
    fi

    cd "$EXSEARCH_PATH"

    log "mix clean"
    mix clean

    if [ -n "$LOCKFILE" -a -f "mix.lock" ]
    then
        log "rm mix.lock"
        rm -f mix.lock
    fi

    cd -
}

clean_fssearch () {
    echo
    hdr "clean_fssearch"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("fssearch")
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

    clean_json_resources "$FSSEARCH_PATH/FsSearchLib/Resources"

    clean_test_resources "$FSSEARCH_PATH/FsSearchTests/Resources"

    cd -
}

clean_gosearch () {
    echo
    hdr "clean_gosearch"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        FAILED_BUILDS+=("gosearch")
        return
    fi

    cd "$GOSEARCH_PATH"

    log "go clean"
    go clean

    cd -
}

clean_groovysearch () {
    echo
    hdr "clean_groovysearch"

    cd "$GROOVYSEARCH_PATH"

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
        FAILED_BUILDS+=("groovysearch")
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    clean_json_resources "$GROOVYSEARCH_PATH/src/main/resources"

    clean_test_resources "$GROOVYSEARCH_PATH/src/test/resources"

    cd -
}

clean_hssearch () {
    echo
    hdr "clean_hssearch"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        FAILED_BUILDS+=("hssearch")
        return
    fi

    cd "$HSSEARCH_PATH"

    log "stack clean"
    stack clean

    clean_json_resources "$HSSEARCH_PATH/data"

    if [ -n "$LOCKFILE" -a -f "stack.yaml.lock" ]
    then
        log "rm stack.yaml.lock"
        rm -f stack.yaml.lock
    fi

    cd -
}

clean_javasearch () {
    echo
    hdr "clean_javasearch"

    cd "$JAVASEARCH_PATH"

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
        FAILED_BUILDS+=("javasearch")
        cd -
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    clean_json_resources "$JAVASEARCH_PATH/lib/src/main/resources"

    clean_test_resources "$JAVASEARCH_PATH/lib/src/test/resources"

    cd -
}

clean_jssearch () {
    echo
    hdr "clean_jssearch"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("jssearch")
        return
    fi

    cd $JSSEARCH_PATH

    log "npm run clean"
    npm run clean

    clean_json_resources "$JSSEARCH_PATH/data"

    if [ -n "$LOCKFILE" -a -f "package-lock.json" ]
    then
        log "rm package-lock.json"
        rm -f package-lock.json
    fi

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
        FAILED_BUILDS+=("ktsearch")
        cd -
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    clean_json_resources "$KTSEARCH_PATH/src/main/resources"

    clean_test_resources "$KTSEARCH_PATH/src/test/resources"

    cd -
}

clean_objcsearch () {
    echo
    hdr "clean_objcsearch"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("objcsearch")
        return
    fi

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

clean_phpsearch () {
    echo
    hdr "clean_phpsearch"

    clean_json_resources "$PHPSEARCH_PATH/resources"

    if [ -n "$LOCKFILE" -a -f "$PHPSEARCH_PATH/composer.lock" ]
    then
        log "rm composer.lock"
        rm -f "$PHPSEARCH_PATH/composer.lock"
    fi
}

clean_plsearch () {
    echo
    hdr "clean_plsearch"

    clean_json_resources "$PLSEARCH_PATH/share"
}

clean_ps1search () {
    echo
    hdr "clean_ps1search"
    log "Nothing to do for powershell"
    # TODO: do we want to uninstall?
}

clean_pysearch () {
    echo
    hdr "clean_pysearch"

    clean_json_resources "$PYSEARCH_PATH/pysearch/data"
}

clean_rbsearch () {
    echo
    hdr "clean_rbsearch"

    clean_json_resources "$RBSEARCH_PATH/data"

    clean_test_resources "$RBSEARCH_PATH/test/fixtures"

    if [ -n "$LOCKFILE" -a -f "$RBSEARCH_PATH/Gemfile.lock" ]
    then
        log "rm Gemfile.lock"
        rm -f "$RBSEARCH_PATH/Gemfile.lock"
    fi
}

clean_rssearch () {
    echo
    hdr "clean_rssearch"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo/rust"
        FAILED_BUILDS+=("rssearch")
        return
    fi

    cd "$RSSEARCH_PATH"

    echo "cargo clean"
    cargo clean

    if [ -n "$LOCKFILE" -a -f "Cargo.lock" ]
    then
        log "rm Cargo.lock"
        rm -f Cargo.lock
    fi

    cd -
}

clean_scalasearch () {
    echo
    hdr "clean_scalasearch"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install sbt"
        FAILED_BUILDS+=("scalasearch")
        return
    fi

    # TODO: convert to sbt command

    cd "$SCALASEARCH_PATH"

    log "sbt clean"
    sbt clean

    clean_json_resources "$SCALASEARCH_PATH/src/main/resources"

    clean_test_resources "$SCALASEARCH_PATH/src/test/resources"

    cd -
}

clean_swiftsearch () {
    echo
    hdr "clean_swiftsearch"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("swiftsearch")
        return
    fi

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
        log_error "You need to install npm"
        FAILED_BUILDS+=("tssearch")
        return
    fi

    cd "$TSSEARCH_PATH"

    log "npm run clean"
    npm run clean

    clean_json_resources "$TSSEARCH_PATH/data"

    if [ -n "$LOCKFILE" -a -f "package-lock.json" ]
    then
        log "rm package-lock.json"
        rm -f package-lock.json
    fi

    cd -
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

    # clean_bashsearch

    # clean_csearch

    clean_cljsearch

    clean_cppsearch

    clean_cssearch

    clean_dartsearch

    clean_exsearch

    clean_fssearch

    clean_gosearch

    clean_groovysearch

    clean_hssearch

    clean_javasearch

    clean_jssearch

    clean_ktsearch

    clean_objcsearch

    clean_mlsearch

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
# GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
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
        groovy)
            clean_groovysearch
            ;;
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
        ps1 | pwsh | powershell)
            clean_powershell
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

print_failed_builds
