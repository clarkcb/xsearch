#!/bin/bash
################################################################################
#
# build.sh
#
# Builds specified language version of xsearch, or all versions
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
    echo -e "\nUsage: build.sh [-h|--help] [--debug] [--release] [--venv] {\"all\" | lang [lang...]}\n"
    exit
}

# copy_config_json_resources
copy_config_json_resources () {
    local resources_path="$1"
    log "cp $XSEARCH_SHARED_PATH/config.json $resources_path/"
    cp "$XSEARCH_SHARED_PATH/config.json" "$resources_path/"
}

# copy_searchoptions_json_resources
copy_searchoptions_json_resources () {
    local resources_path="$1"
    log "cp $XSEARCH_SHARED_PATH/searchoptions.json $resources_path/"
    cp "$XSEARCH_SHARED_PATH/searchoptions.json" "$resources_path/"
}

# copy_json_resources
copy_json_resources () {
    local resources_path="$1"
    copy_config_json_resources "$resources_path"
    copy_searchoptions_json_resources "$resources_path"
}

# copy_test_resources
copy_test_resources () {
    local test_resources_path="$1"
    log "cp $XSEARCH_TEST_FILE_PATH/testFile*.txt $test_resources_path/"
    cp "$XSEARCH_TEST_FILE_PATH"/testFile*.txt "$test_resources_path/"
}

# add_to_bin
add_to_bin () {
    local script_path="$1"
    local script_name=$(basename "$1")
    if [ ! -d "$XSEARCH_BIN_PATH" ]
    then
        log "Creating bin path"
        log "mkdir -p $XSEARCH_BIN_PATH"
        mkdir -p "$XSEARCH_BIN_PATH"
    fi

    cd "$XSEARCH_BIN_PATH"

    if [[ $script_name == *.sh || $script_name == *.bash || $script_name == *.ps1 ]]
    then
        script_name=${script_name%%.*}
    fi

    # echo "script_name: $script_name"
    # if [ -L "$script_name" ]
    # then
    #     log "rm $script_name"
    #     rm "$script_name"
    # fi

    log "ln -sf $script_path $script_name"
    ln -sf "$script_path" "$script_name"

    cd -
}

print_failed_builds () {
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed builds: ${FAILED_BUILDS[*]}"
    else
        log "All builds succeeded"
    fi
}

########################################
# Build Functions
########################################

build_bashsearch () {
    echo
    hdr "build_bashsearch"
    log "language: bash"
    log "Not currently implemented"
}

build_csearch () {
    echo
    hdr "build_csearch"
    log "language: C"

    # ensure make is installed
    if [ -z "$(which make)" ]
    then
        log_error "You need to install make"
        FAILED_BUILDS+=("csearch")
        return
    fi

    cd "$CSEARCH_PATH"

    # make
    log "Building csearch"
    log "make"
    make

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("csearch")
        return
    fi

    # add to bin
    add_to_bin "$CSEARCH_PATH/csearch"

    cd -
}

build_cljsearch () {
    echo
    hdr "build_cljsearch"
    log "language: clojure"

    # ensure clojure is installed
    if [ -z "$(which clj)" ]
    then
        log_error "You need to install clojure"
        FAILED_BUILDS+=("cljsearch")
        return
    fi

    # clj -version output looks like this: Clojure CLI version 1.11.4.1474
    # CLOJURE_VERSION=$(clj -version | head -n 1 | cut -d ' ' -f 3)
    CLOJURE_VERSION=$(clj -version 2>&1)
    log "clojure version: $CLOJURE_VERSION"

    # ensure leiningen is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install leiningen"
        FAILED_BUILDS+=("cljsearch")
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$CLJSEARCH_PATH/resources"
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    cd "$CLJSEARCH_PATH"

    # Create uberjar with lein
    log "Building cljsearch"
    log "lein clean"
    lein clean

    # install to local maven repository
    log "lein install"
    lein install

    # create uberjar
    log "lein uberjar"
    lein uberjar

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("cljsearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$CLJSEARCH_PATH/bin/cljsearch.sh"

    cd -
}

build_cppsearch () {
    echo
    hdr "build_cppsearch"
    log "language: C++"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        FAILED_BUILDS+=("cppsearch")
        return
    fi

    # cmake --version output looks like this: cmake version 3.30.2
    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    cd "$CPPSEARCH_PATH"

    # CMAKE_CXX_FLAGS="-W -Wall -Werror"
    CMAKE_CXX_FLAGS="-W -Wall -Werror -Wextra -Wshadow -Wnon-virtual-dtor -pedantic"

    # Add AddressSanitizer
    # CMAKE_CXX_FLAGS="$CMAKE_CXX_FLAGS -fsanitize=address -fno-omit-frame-pointer"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(debug release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(release)
    fi

    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"
        CMAKE_BUILD_PATH="$CPPSEARCH_PATH/$CMAKE_BUILD_DIR"
        CMAKE_BUILD_TYPE="$c"

        if [ ! -d "$CMAKE_BUILD_PATH" ]
        then
            log "mkdir -p $CMAKE_BUILD_PATH"
            mkdir -p "$CMAKE_BUILD_PATH"

            log "cd $CMAKE_BUILD_PATH"
            cd "$CMAKE_BUILD_PATH"

            log "cmake -G \"Unix Makefiles\" -DCMAKE_BUILD_TYPE=$c .."
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # exec 5>&1
            # log "make -f Makefile"
            # OUTPUT=$(make -f Makefile | tee >(cat - >&5))
            # I=$(echo "$OUTPUT" | grep "\[100%\] Built target ")
            # make -f Makefile

            cd -
        fi

        if [ -d "$CMAKE_BUILD_PATH" ]
        then
            TARGETS=(clean cppsearch cppsearchapp cppsearch-tests)
            for t in ${TARGETS[*]}
            do
                log "cmake --build $CMAKE_BUILD_DIR --config $c --target $t -- $CMAKE_CXX_FLAGS"
                cmake --build "$CMAKE_BUILD_DIR" --config "$c" --target "$t" -- "$CMAKE_CXX_FLAGS"

                # check for success/failure
                # [ "$?" -ne 0 ] && log "An error occurred while trying to run build target $t" >&2 && exit 1
                if [ "$?" -eq 0 ]
                then
                    log "Build target $t succeeded"
                else
                    log_error "Build target $t failed"
                    FAILED_BUILDS+=("cppsearch")
                    cd -
                    return
                fi
            done
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$CPPSEARCH_PATH/bin/cppsearch.release.sh"
    else
        # add debug to bin
        add_to_bin "$CPPSEARCH_PATH/bin/cppsearch.debug.sh"
    fi

    cd -
}

build_cssearch () {
    echo
    hdr "build_cssearch"
    log "language: C#"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("cssearch")
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    RESOURCES_PATH="$CSSEARCH_PATH/CsSearchLib/Resources"
    TEST_RESOURCES_PATH="$CSSEARCH_PATH/CsSearchTests/Resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Debug Release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(Debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Release)
    fi

    # run dotnet build for selected configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building cssearch for $c configuration"
        log "dotnet build $CSSEARCH_PATH/CsSearch.sln --configuration $c"
        dotnet build "$CSSEARCH_PATH/CsSearch.sln" --configuration "$c"

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("cssearch")
            return
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$CSSEARCH_PATH/bin/cssearch.release.sh"
    else
        # add debug to bin
        add_to_bin "$CSSEARCH_PATH/bin/cssearch.debug.sh"
    fi
}

build_dartsearch () {
    echo
    hdr "build_dartsearch"
    log "language: dart"

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

    # RESOURCES_PATH=$DARTSEARCH_PATH/lib/data

    # TODO: move resources to local location, for now read relative to XSEARCH_PATH
    # mkdir -p "$RESOURCES_PATH"
    # copy_json_resources "$RESOURCES_PATH"

    log "Building dartsearch"
    if [ ! -f "$DARTSEARCH_PATH/.dart_tool/package_config.json" ] && [ ! -f "$DARTSEARCH_PATH/.packages" ]
    then
        log "dart pub get"
        dart pub get
    else
        log "dart pub upgrade"
        dart pub upgrade
    fi

    log "Compiling dartsearch"
    log "dart compile exe $DARTSEARCH_PATH/bin/dartsearch.dart"
    dart compile exe "$DARTSEARCH_PATH/bin/dartsearch.dart"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("dartsearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$DARTSEARCH_PATH/bin/dartsearch.sh"

    cd -
}

build_exsearch () {
    echo
    hdr "build_exsearch"
    log "language: elixir"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        FAILED_BUILDS+=("exsearch")
        return
    fi

    ELIXIR_VERSION=$(elixir --version | grep Elixir)
    log "elixir version: $ELIXIR_VERSION"

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

    log "Building exsearch"
    log "mix deps.get"
    mix deps.get

    log "Compiling exfind"
    log "mix compile"
    mix compile

    log "Creating exsearch executable"
    log "mix escript.build"
    mix escript.build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("exsearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$EXSEARCH_PATH/bin/exsearch"

    cd -
}

build_fssearch () {
    echo
    hdr "build_fssearch"
    log "language: F#"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("fssearch")
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    RESOURCES_PATH="$FSSEARCH_PATH/FsSearchLib/Resources"
    TEST_RESOURCES_PATH="$FSSEARCH_PATH/FsSearchTests/Resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Debug Release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(Debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Release)
    fi

    # run dotnet build for selected configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building fssearch for $c configuration"
        log "dotnet build $FSSEARCH_PATH/FsSearch.sln --configuration $c"
        dotnet build "$FSSEARCH_PATH/FsSearch.sln" --configuration "$c"

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("fssearch")
            return
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$FSSEARCH_PATH/bin/fssearch.release.sh"
    else
        # add debug to bin
        add_to_bin "$FSSEARCH_PATH/bin/fssearch.debug.sh"
    fi
}

build_gosearch () {
    echo
    hdr "build_gosearch"
    log "language: go"

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

    # build the code to generate the dynamic code for gosearch
    #log "Building gengosearchcode"
    #echo "go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode"
    #go install elocale.com/clarkcb/gosearchcodegen/gengosearchcode

    # run it to generate the dynamic gosearch code
    #log "Running gengosearchcode"
    #log "gengosearchcode"
    #gengosearchcode

    cd "$GOSEARCH_PATH"

    # go fmt the gosearch source (for auto-generated code)
    log "Auto-formatting gosearch"
    log "go fmt ./..."
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if [ ! -d "$XSEARCH_BIN_PATH" ]
    then
        mkdir -p "$XSEARCH_BIN_PATH"
    fi

    # if GOBIN not defined, set to XSEARCH_BIN_PATH
    # if [ ! -d "$GOBIN" ]
    # then
    #     export GOBIN="$XSEARCH_BIN_PATH"
    # fi

    # now build/install gosearch
    log "Building gosearch"
    log "go install ./..."
    GOBIN="$XSEARCH_BIN_PATH" go install ./...

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("gosearch")
    fi

    cd -
}

build_hssearch () {
    echo
    hdr "build_hssearch"
    log "language: haskell"

    # ensure ghc is installed
    if [ -z "$(which ghc)" ]
    then
        log_error "You need to install ghc"
        FAILED_BUILDS+=("hssearch")
        return
    fi

    GHC_VERSION=$(ghc --version)
    log "ghc version: $GHC_VERSION"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        FAILED_BUILDS+=("hssearch")
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"

    # set the default stack settings, e.g. use system ghc
    STACK_DIR=$HOME/.stack
    if [ ! -d "$STACK_DIR" ]
    then
        mkdir -p "$STACK_DIR"
    fi
    if [ ! -f "$STACK_DIR/config.yaml" ]
    then
        touch "$STACK_DIR/config.yaml"
    fi
    INSTALL_GHC=$(grep '^install-ghc:' "$STACK_DIR"/config.yaml)
    if [ -z "$INSTALL_GHC" ]
    then
        echo 'install-ghc: false' >> "$STACK_DIR/config.yaml"
    fi
    SYSTEM_GHC=$(grep '^system-ghc:' "$STACK_DIR"/config.yaml)
    if [ -z "$SYSTEM_GHC" ]
    then
        echo 'system-ghc: true' >> "$STACK_DIR/config.yaml"
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$HSSEARCH_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    cd "$HSSEARCH_PATH/"

    # build with stack (via make)
    log "Building hssearch"
    log "stack setup"
    make setup

    log "stack build"
    make build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("hssearch")
        cd -
        return
    fi

    log "stack install --local-bin-path $XSEARCH_BIN_PATH"
    stack install --local-bin-path "$XSEARCH_BIN_PATH"

    cd -
}

build_javasearch () {
    echo
    hdr "build_javasearch"
    log "language: java"

    # ensure java is installed
    if [ -z "$(which java)" ]
    then
        log_error "You need to install java"
        FAILED_BUILDS+=("javasearch")
        return
    fi

    JAVA_VERSION=$(java -version 2>&1 | head -n 1)
    log "java version: $JAVA_VERSION"

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

    GRADLE_OUTPUT=$($GRADLE --version)
    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    RESOURCES_PATH="$JAVASEARCH_PATH/lib/src/main/resources"
    TEST_RESOURCES_PATH="$JAVASEARCH_PATH/lib/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # run a gradle clean jar build
    log "Building javasearch"

    # log "gradle --warning-mode all clean jar publishToMavenLocal"
    # gradle --warning-mode all clean jar publishToMavenLocal
    # GRADLE_ARGS="--info --warning-mode all"
    GRADLE_ARGS="--warning-mode all"
    GRADLE_TASKS=(clean :lib:jar :lib:publishToMavenLocal :app:jar)
    for t in ${GRADLE_TASKS[*]}
    do
        log "$GRADLE $GRADLE_ARGS $t"
        "$GRADLE" --warning-mode all $t
    done

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("javasearch")
        cd -
        return
    fi

    # # install to local repo so it can be added as a dependency to javasearch
    # log "mvn -f $JAVASEARCH_PATH/pom.xml install"
    # mvn -f "$JAVASEARCH_PATH/pom.xml" install

    # add to bin
    add_to_bin "$JAVASEARCH_PATH/bin/javasearch.sh"

    cd -
}

build_jssearch () {
    echo
    hdr "build_jssearch"
    log "language: javascript"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node.js"
        FAILED_BUILDS+=("jssearch")
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("jssearch")
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$JSSEARCH_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_config_json_resources "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    cd "$JSSEARCH_PATH"

    # run npm install and build
    log "Building jssearch"
    log "npm install"
    npm install

    log "npm run build"
    npm run build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("jssearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$JSSEARCH_PATH/bin/jssearch.sh"

    cd -
}

build_ktsearch () {
    echo
    hdr "build_ktsearch"
    log "language: kotlin"

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

    GRADLE_OUTPUT=$($GRADLE --version)
    # echo "$GRADLE_OUTPUT"

    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    RESOURCES_PATH="$KTSEARCH_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$KTSEARCH_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # TEMP(?): copy the jar file for the local ktfind dependency to lib
    # log "KTFIND_PATH: $KTFIND_PATH"
    # KTFIND_JAR=$(find "$KTFIND_PATH/build/libs" -maxdepth 1 -name "ktfind*.jar" | head -n 1)
    # if [ -f "$KTFIND_JAR" ]
    # then
    #     log "cp $KTFIND_JAR $KTSEARCH_PATH/lib/"
    #     cp "$KTFIND_JAR" "$KTSEARCH_PATH/lib/"
    # fi

    # run a gradle clean jar build
    log "Building ktsearch"

    # log "gradle --warning-mode all clean jar publishToMavenLocal"
    # gradle --warning-mode all clean jar publishToMavenLocal
    # GRADLE_ARGS="--info --warning-mode all"
    GRADLE_ARGS="--warning-mode all"
    GRADLE_TASKS=(clean :lib:jar :lib:publishToMavenLocal :app:jar)
    for t in ${GRADLE_TASKS[*]}
    do
        log "$GRADLE $GRADLE_ARGS $t"
        "$GRADLE" --warning-mode all $t
    done

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("ktsearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$KTSEARCH_PATH/bin/ktsearch.sh"

    cd -
}

build_objcsearch () {
    echo
    hdr "build_objcsearch"
    log "language: Objective C"

    TARGET=alltargets

    # TODO: copy resource files locally?
    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("objcsearch")
        return
    fi

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    SWIFT_VERSION=$(swift --version 2>&1 | grep 'Apple Swift' | cut -d ' ' -f 7)
    log "swift version: Apple Swift version $SWIFT_VERSION"

    # TODO: copy resource files locally? - embedded resources not currently supported apparently

    cd "$OBJCSEARCH_PATH"

    # run swift build
    log "Building objcsearch"

    if [ -n "$DEBUG" ]
    then
        log "swift build"
        swift build

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("objcsearch")
            cd -
            return
        fi
    fi
    if [ -n "$RELEASE" ]
    then
        log "swift build --configuration release"
        swift build --configuration release

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("objcsearch")
            cd -
            return
        fi

        # add release to bin
        add_to_bin "$OBJCSEARCH_PATH/bin/objcsearch.release.sh"
    else
        # add debug to bin
        add_to_bin "$OBJCSEARCH_PATH/bin/objcsearch.debug.sh"
    fi

    cd -
}

build_mlsearch () {
    echo
    hdr "build_mlsearch"
    log "language: ocaml"

    cd "$MLSEARCH_PATH"
    ./build.sh
    # if [ -L ~/bin/mlsearch ]
    # then
    #     rm ~/bin/mlsearch
    # fi
    ln -s "$MLSEARCH_PATH/_build/src/mlsearch.native" ~/bin/mlsearch
    cd -
}

build_plsearch () {
    echo
    hdr "build_plsearch"
    log "language: perl"

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

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$PLSEARCH_PATH/share"
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("plsearch")
        return
    fi

    # add to bin
    add_to_bin "$PLSEARCH_PATH/bin/plsearch.sh"
}

build_phpsearch () {
    echo
    hdr "build_phpsearch"
    log "language: php"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        FAILED_BUILDS+=("phpsearch")
        return
    fi

    # PHP_VERSION=$(php -r "echo phpversion();")
    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        FAILED_BUILDS+=("phpsearch")
        return
    fi
    log "php version: $PHP_VERSION"

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        log_error "Need to install composer"
        FAILED_BUILDS+=("phpsearch")
        return
    fi

    COMPOSER_VERSION=$(composer --version 2>&1 | grep '^Composer')
    log "composer version: $COMPOSER_VERSION"

    CONFIG_PATH="$PHPSEARCH_PATH/config"
    RESOURCES_PATH="$PHPSEARCH_PATH/resources"

    # copy the shared config json file to the local config location
    mkdir -p "$CONFIG_PATH"
    log "cp $XSEARCH_SHARED_PATH/config.json $CONFIG_PATH/"
    cp "$XSEARCH_SHARED_PATH/config.json" "$CONFIG_PATH/"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    log "cp $XSEARCH_SHARED_PATH/searchoptions.json $RESOURCES_PATH/"
    cp "$XSEARCH_SHARED_PATH/searchoptions.json" "$RESOURCES_PATH/"

    cd "$PHPSEARCH_PATH"

    # run a composer build
    log "Building phpsearch"

    if [ -d "$PHPSEARCH_PATH/vendor" ]
    then
        log "composer update"
        composer update
    else
        log "composer install"
        composer install
    fi

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("phpsearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$PHPSEARCH_PATH/bin/phpsearch.sh"

    cd -
}

build_ps1search () {
    echo
    hdr "build_ps1search"
    log "language: powershell"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        FAILED_BUILDS+=("ps1search")
        return
    fi

    POWERSHELL_VERSION=$(pwsh -v)
    log "powershell version: $POWERSHELL_VERSION"

    MODULEPATH=$(pwsh -c 'echo $env:PSModulePath')
    if [ -z "$MODULEPATH" ]
    then
        log_error "Unable to get powershell module path"
        FAILED_BUILDS+=("ps1search")
        return
    fi

    log "Building ps1search"

    # split on : and get the first path
    IFS=':' read -ra MODULEPATHS <<< "$MODULEPATH"
    MODULEPATH=${MODULEPATHS[0]}
    PS1SEARCHMODULEPATH="$MODULEPATH/Ps1SearchModule"

    mkdir -p "$PS1SEARCHMODULEPATH"

    log "cp $PS1SEARCH_PATH/Ps1SearchModule.psm1 $PS1SEARCHMODULEPATH/"
    cp "$PS1SEARCH_PATH/Ps1SearchModule.psm1" "$PS1SEARCHMODULEPATH/"

    # add to bin
    add_to_bin "$PS1SEARCH_PATH/ps1search.ps1"
}

build_pysearch () {
    echo
    hdr "build_pysearch"
    log "language: python"

    # Set to Yes to use venv
    USE_VENV=$VENV
    # PYTHON_VERSIONS=(python3.12 python3.11 python3.10 python3.9)
    # We don't want to use python3.12 yet
    PYTHON_VERSIONS=(python3.11 python3.10 python3.9)
    PYTHON=

    ACTIVE_VENV=

    if [ "$USE_VENV" == 'yes' ]
    then
        log 'Using venv'

        # 3 possibilities:
        # 1. venv exists and is active
        # 2. venv exists and is not active
        # 3. venv does not exist

        if [ -n "$VIRTUAL_ENV" ]
        then
            # 1. venv exists and is active
            log "Already active venv: $VIRTUAL_ENV"
            ACTIVE_VENV="$VIRTUAL_ENV"

            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")

        elif [ -d "$PYSEARCH_PATH/venv" ]
        then
            # 2. venv exists and is not active
            log 'Using existing venv'

            # activate the venv - we run this even if this venv or another is already active
            # because it's the only way to be able to run deactivate later
            log "source $PYSEARCH_PATH/venv/bin/activate"
            source $PYSEARCH_PATH/venv/bin/activate

            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")

        else
            # 3. venv does not exist
            # ensure python3.9+ is installed
            for p in ${PYTHON_VERSIONS[*]}
            do
                PYTHON=$(which "$p")
                if [ -f "$PYTHON" ]
                then
                    break
                fi
            done

            if [ -z "$PYTHON" ]
            then
                log_error "A version of python >= 3.9 is required"
                FAILED_BUILDS+=("pysearch")
                return
            else
                PYTHON=$(basename "$PYTHON")
            fi

            log "Creating new venv"

            # create a virtual env to run from and install to if it doesn't already exist
            log "$PYTHON -m venv venv"
            "$PYTHON" -m venv venv

            # activate the venv
            log "source $PYSEARCH_PATH/venv/bin/activate"
            source $PYSEARCH_PATH/venv/bin/activate

            # get the path to the venv version
            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")
        fi

    else

        log "Not using venv"

        # ensure python3.9+ is installed
        for p in ${PYTHON_VERSIONS[*]}
        do
            PYTHON=$(which "$p")
            if [ -f "$PYTHON" ]
            then
                break
            fi
        done

        if [ -z "$PYTHON" ]
        then
            log_error "A version of python >= 3.9 is required"
            FAILED_BUILDS+=("pysearch")
            return
        else
            PYTHON=$(basename "$PYTHON")
        fi
    fi

    log "Using $PYTHON ($(which $PYTHON))"
    log "$PYTHON -V: $($PYTHON -V)"

    # # copy the shared json files to the local resource location
    RESOURCES_PATH="$PYSEARCH_PATH/pysearch/data"
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    cd "$PYSEARCH_PATH"

    # install wheel - this seems to fix problems with installing local dependencies,
    # which pyfind will be for pysearch
    # log "pip3 install wheel"
    # pip3 install wheel

    # install dependencies in requirements.txt
    log "pip3 install -r requirements.txt"
    pip3 install -r requirements.txt

    # check for success/failure
    ERROR=
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("pysearch")
        ERROR=yes
    fi

    # if there was not an active venv before the build, deactivate the venv
    if [ "$USE_VENV" == 'yes' -a -z "$ACTIVE_VENV" ]
    then
        # deactivate at end of setup process
        log "deactivate"
        deactivate
    fi

    if [ -n "$ERROR" ]
    then
        cd -
        return
    fi

    # TODO: change the !# line in pysearch to use the determined python version

    # add to bin
    add_to_bin "$PYSEARCH_PATH/bin/pysearch.sh"

    cd -
}

build_rbsearch () {
    echo
    hdr "build_rbsearch"
    log "language: ruby"

    # ensure ruby2.x+ is installed
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

    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        FAILED_BUILDS+=("rbsearch")
        return
    fi

    BUNDLE_VERSION="$(bundle version)"
    log "$BUNDLE_VERSION"

    RESOURCES_PATH="$RBSEARCH_PATH/data"
    TEST_RESOURCES_PATH="$RBSEARCH_PATH/test/fixtures"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # TODO: figure out how to install dependencies without installing rbsearch (which is what bundler does)
    cd "$RBSEARCH_PATH"

    log "Building rbsearch"
    log "bundle install"
    bundle install

    # check for success/failure
    if [ "$?" -ne 0 ]
    then
        log_error "bundle install failed"
        FAILED_BUILDS+=("rbsearch")
        cd -
        return
    fi

    # TODO: install the gem?
    # log "gem install rbsearch-0.1.0.gem"
    # gem install rbsearch-0.1.0.gem

    # add to bin
    add_to_bin "$RBSEARCH_PATH/bin/rbsearch.sh"

    cd -
}

build_rssearch () {
    echo
    hdr "build_rssearch"
    log "language: rust"

    # ensure rust is installed
    if [ -z "$(which rustc)" ]
    then
        log_error "You need to install rust"
        FAILED_BUILDS+=("rssearch")
        return
    fi

    RUST_VERSION=$(rustc --version)
    log "rustc version: $RUST_VERSION"

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

    log "Building rssearch"
    if [ -n "$DEBUG" ]
    then
        log "cargo build"
        cargo build

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("rssearch")
            cd -
            return
        fi
    fi
    if [ -n "$RELEASE" ]
    then
        log "cargo build --release"
        cargo build --release

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("rssearch")
            cd -
            return
        fi

        # add release to bin
        add_to_bin "$RSSEARCH_PATH/bin/rssearch.release.sh"
    else
        # add debug to bin
        add_to_bin "$RSSEARCH_PATH/bin/rssearch.debug.sh"
    fi

    cd -
}

build_scalasearch () {
    echo
    hdr "build_scalasearch"
    log "language: scala"

    # ensure sbt is installed
    if [ -z "$(which scala)" ]
    then
        log_error "You need to install scala"
        FAILED_BUILDS+=("scalasearch")
        return
    fi

    # scala --version output looks like this:
    # Scala code runner version: 1.4.3
    # Scala version (default): 3.5.2
    SCALA_VERSION=$(scala -version 2>&1 | tail -n 1 | cut -d ' ' -f 4)
    log "scala version: $SCALA_VERSION"

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

    RESOURCES_PATH="$SCALASEARCH_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$SCALASEARCH_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # TEMP(?): copy the jar file for the local scalafind dependency to lib
    if [ -z "$SCALA_VERSION" ]
    then
        SCALA_VERSION=3.5.2
    fi
    if [ -z "$SCALAFIND_PATH" ]
    then
        SCALAFIND_PATH="$HOME/src/xfind/scala/scalafind"
    fi
    SCALAFIND_JAR=$(find "$SCALAFIND_PATH/target/scala-$SCALA_VERSION" -maxdepth 1 -name "scalafind*.jar" | grep -v assembly | head -n 1)
    if [ -f "$SCALAFIND_JAR" ]
    then
        log "cp $SCALAFIND_JAR $SCALASEARCH_PATH/lib/"
        cp "$SCALAFIND_JAR" "$SCALASEARCH_PATH/lib/"
    fi

    cd "$SCALASEARCH_PATH"

    # run sbt assembly
    log "Building scalasearch"
    # log "sbt clean assembly"
    # sbt clean assembly
    # to build without testing, changed to this:
    log "sbt 'set test in assembly := {}' clean assembly"
    sbt 'set test in assembly := {}' clean assembly

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("scalasearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$SCALASEARCH_PATH/bin/scalasearch.sh"

    cd -
}

build_swiftsearch () {
    echo
    hdr "build_swiftsearch"
    log "language: swift"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("swiftsearch")
        return
    fi

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    SWIFT_VERSION=$(swift --version 2>&1 | grep 'Apple Swift' | cut -d ' ' -f 7)
    log "swift version: Apple Swift version $SWIFT_VERSION"

    # TODO: copy resource files locally? - embedded resources not currently supported apparently

    cd "$SWIFTSEARCH_PATH"

    # run swift build
    log "Building swiftsearch"

    if [ -n "$DEBUG" ]
    then
        log "swift build"
        swift build

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("swiftsearch")
            cd -
            return
        fi
    fi
    if [ -n "$RELEASE" ]
    then
        log "swift build --configuration release"
        swift build --configuration release

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            FAILED_BUILDS+=("swiftsearch")
            cd -
            return
        fi

        # add release to bin
        add_to_bin "$SWIFTSEARCH_PATH/bin/swiftsearch.release.sh"
    else
        # add debug to bin
        add_to_bin "$SWIFTSEARCH_PATH/bin/swiftsearch.debug.sh"
    fi

    cd -
}

build_tssearch () {
    echo
    hdr "build_tssearch"
    log "language: typescript"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node.js"
        FAILED_BUILDS+=("tssearch")
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("tssearch")
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$TSSEARCH_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_json_resources "$RESOURCES_PATH"

    cd "$TSSEARCH_PATH"

    # run npm install and build
    log "Building tssearch"
    log "npm install"
    npm install
    log "npm run build"
    npm run build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        FAILED_BUILDS+=("tssearch")
        cd -
        return
    fi

    # add to bin
    add_to_bin "$TSSEARCH_PATH/bin/tssearch.sh"

    cd -
}

# build_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - build_cljsearch    - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - build_cppsearch    - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - build_gosearch     - go is known for having very fast builds, and it's true, the only builds that are faster here
#                        are the ones that do nothing except copy over resources files (e.g. perl)
# - build_hssearch     - having some dependency issues that need to work through to get it buildling again
# - build_jssearch     - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                        in an external terminal fixes the problem
# - build_ktsearch     - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                        time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                        in a while
# - build_objcsearch   - not sure if it's even possible to build this on linux, but excluding for now
# - build_mlsearch     - had a number of different issues trying to get this version building again, finally
#                        gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - build_rssearch     - the first time this build is run it will pretty time-consuming, particularly for release
#                        target, but intermittent builds should be pretty fast
# - build_scalasearch  - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - build_tssearch     - this build has the same problem as build_jssearch; run the build in an external terminal
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

    time build_groovysearch

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
# GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

HELP=
DEBUG=
RELEASE=
VENV=
BUILD_ALL=
# TARGET_LANG=all
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
            build_linux
            ;;
        # bash)
        #     time build_bashsearch
        #     ;;
        # c)
        #     build_csearch
        #     ;;
        clj | clojure)
            build_cljsearch
            ;;
        cpp)
            build_cppsearch
            ;;
        cs | csharp)
            build_cssearch
            ;;
        dart)
            build_dartsearch
            ;;
        ex | elixir)
            build_exsearch
            ;;
        fs | fsharp)
            build_fssearch
            ;;
        go)
            build_gosearch
            ;;
        # groovy)
        #     build_groovysearch
        #     ;;
        haskell | hs)
            build_hssearch
            ;;
        java)
            build_javasearch
            ;;
        javascript | js)
            build_jssearch
            ;;
        kotlin | kt)
            build_ktsearch
            ;;
        objc)
            build_objcsearch
            ;;
        # ocaml | ml)
        #     build_mlsearch
        #     ;;
        perl | pl)
            build_plsearch
            ;;
        php)
            build_phpsearch
            ;;
        ps1 | powershell | pwsh)
            build_ps1search
            ;;
        py | python)
            build_pysearch
            ;;
        rb | ruby)
            build_rbsearch
            ;;
        rs | rust)
            build_rssearch
            ;;
        scala)
            build_scalasearch
            ;;
        swift)
            build_swiftsearch
            ;;
        ts | typescript)
            build_tssearch
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_failed_builds
