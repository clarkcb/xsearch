#!/bin/sh
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


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: build.sh [-h|--help] [--debug] [--release] [--venv] {\"all\" | langcode}\n"
    exit
}

# copy_searchoptions_resources
copy_searchoptions_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/searchoptions.json $resources_path/"
    cp "$SHARED_PATH/searchoptions.json" "$resources_path/"
}

# copy_json_resources
copy_json_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/config.json $resources_path/"
    cp "$SHARED_PATH/config.json" "$resources_path/"
    log "cp $SHARED_PATH/filetypes.json $resources_path/"
    cp "$SHARED_PATH/filetypes.json" "$resources_path/"
    log "cp $SHARED_PATH/searchoptions.json $resources_path/"
    cp "$SHARED_PATH/searchoptions.json" "$resources_path/"
}

# copy_xml_resources
copy_xml_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/filetypes.xml $resources_path/"
    cp "$SHARED_PATH/filetypes.xml" "$resources_path/"
    log "cp $SHARED_PATH/searchoptions.xml $resources_path/"
    cp "$SHARED_PATH/searchoptions.xml" "$resources_path/"
}

# copy_test_resources
copy_test_resources () {
    local test_resources_path="$1"
    log "cp $TEST_FILE_PATH/testFile*.txt $test_resources_path/"
    cp "$TEST_FILE_PATH/testFile*.txt" "$test_resources_path/"
}

# add_to_bin
add_to_bin () {
    local script_path="$1"
    local script_name=$(basename "$1")
    if [ ! -d "$BIN_PATH" ]
    then
        log "Creating bin path"
        log "mkdir -p $BIN_PATH"
        mkdir -p "$BIN_PATH"
    fi

    cd "$BIN_PATH"

    if [[ $script_name == *.sh ]]
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

########################################
# Build Functions
########################################

build_c () {
    echo
    hdr "build_c"

    # ensure make is installed
    if [ -z "$(which make)" ]
    then
        echo "You need to install make"
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
        return
    fi

    # add to bin
    add_to_bin "$CSEARCH_PATH/csearch"

    cd -
}

build_clojure () {
    echo
    hdr "build_clojure"

    # ensure leiningen is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install leiningen"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$CLJSEARCH_PATH/resources"
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_resources "$RESOURCES_PATH"

    cd "$CLJSEARCH_PATH"

    # Create uberjar with lein
    log "Building cljsearch"
    log "lein clean"
    lein clean
    log "lein uberjar"
    lein uberjar

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$CLJSEARCH_PATH/bin/cljsearch.sh"

    cd -
}

build_cpp () {
    echo
    hdr "build_cpp"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        echo "You need to install cmake"
        return
    fi

    cd "$CPPSEARCH_PATH"

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
                log "cmake --build $CMAKE_BUILD_DIR --config $c --target $t -- -W -Wall -Werror"
                cmake --build "$CMAKE_BUILD_DIR" --config "$c" --target "$t" -- -W -Wall -Werror

                # check for success/failure
                # [ "$?" -ne 0 ] && log "An error occurred while trying to run build target $t" >&2 && exit 1
                if [ "$?" -eq 0 ]
                then
                    log "Build succeeded"
                else
                    log_error "Build failed"
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

build_csharp () {
    echo
    hdr "build_csharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    RESOURCES_PATH="$CSSEARCH_PATH/CsSearchLib/Resources"
    TEST_RESOURCES_PATH="$CSSEARCH_PATH/CsSearchTests/Resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_resources "$RESOURCES_PATH"

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

build_dart () {
    echo
    hdr "build_dart"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        echo "You need to install dart"
        return
    fi

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

    # add to bin
    add_to_bin "$DARTSEARCH_PATH/bin/dartsearch.sh"

    cd -
}

build_fsharp () {
    echo
    hdr "build_fsharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    RESOURCES_PATH="$FSSEARCH_PATH/FsSearchLib/Resources"
    TEST_RESOURCES_PATH="$FSSEARCH_PATH/FsSearchTests/Resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_resources "$RESOURCES_PATH"

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

build_go () {
    echo
    hdr "build_go"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

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
    if [ ! -d "$BIN_PATH" ]
    then
        mkdir -p "$BIN_PATH"
    fi

    # if GOBIN not defined, set to BIN_PATH
    # if [ ! -d "$GOBIN" ]
    # then
    #     export GOBIN="$BIN_PATH"
    # fi

    # now build/install gosearch
    log "Building gosearch"
    log "go install ./..."
    GOBIN="$BIN_PATH" go install ./...

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    cd -
}

build_haskell () {
    echo
    hdr "build_haskell"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

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
    copy_json_resources "$RESOURCES_PATH"

    cd "$HSSEARCH_PATH/"

    # build with stack (via make)
    log "Building hssearch"
    log "stack setup"
    make setup

    log "stack build"
    make build

    log "stack install --local-bin-path $BIN_PATH"
    stack install --local-bin-path "$BIN_PATH"

    cd -
}

build_java () {
    echo
    hdr "build_java"

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        echo "You need to install maven"
        return
    fi

    RESOURCES_PATH="$JAVASEARCH_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$JAVASEARCH_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # run a maven clean build
    log "Building javasearch"
    log "mvn -f $JAVASEARCH_PATH/pom.xml clean package -Dmaven.test.skip=true -Dmaven.plugin.validation=DEFAULT"
    mvn -f "$JAVASEARCH_PATH/pom.xml" clean package -Dmaven.test.skip=true -Dmaven.plugin.validation=DEFAULT

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$JAVASEARCH_PATH/bin/javasearch.sh"
}

build_javascript () {
    echo
    hdr "build_javascript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install node.js/npm"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$JSSEARCH_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

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
        return
    fi

    # add to bin
    add_to_bin "$JSSEARCH_PATH/bin/jssearch.sh"

    cd -
}

build_kotlin () {
    echo
    hdr "build_kotlin"

    # ensure gradle is installed
    if [ -z "$(which gradle)" ]
    then
        echo "You need to install gradle"
        return
    fi

    RESOURCES_PATH="$KTSEARCH_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$KTSEARCH_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # copy over latest ktfind dependency if not found
    KTFIND_JAR=$(find $KTSEARCH_PATH/lib -name "ktfind*.jar" | head -n 1)
    if [ -z "$KTFIND_JAR" ]
    then
        log_error "You need to copy the ktfind jar into $KTSEARCH_PATH/lib/"
        return
    fi

    # run a gradle build
    log "Building ktsearch"

    cd "$KTSEARCH_PATH/"

    log "gradle --warning-mode all clean jar"
    gradle --warning-mode all clean jar

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$KTSEARCH_PATH/bin/ktsearch.sh"

    cd -
}

build_objc () {
    echo
    hdr "build_objc"

    TARGET=alltargets

    # TODO: copy resource files locally?
    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        return
    fi

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

build_ocaml () {
    echo
    hdr "build_ocaml"

    cd "$MLSEARCH_PATH"
    ./build.sh
    # if [ -L ~/bin/mlsearch ]
    # then
    #     rm ~/bin/mlsearch
    # fi
    ln -s "$MLSEARCH_PATH/_build/src/mlsearch.native" ~/bin/mlsearch
    cd -
}

build_perl () {
    echo
    hdr "build_perl"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        echo "You need to install perl"
        return
    fi

    if [ -z "$(perl -v | grep 'This is perl 5')" ]
    then
        echo "A 5.x version of perl is required"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$PLSEARCH_PATH/share"
    mkdir -p "$RESOURCES_PATH"
    log "cp $SHARED_PATH/config.json $RESOURCES_PATH/"
    cp "$SHARED_PATH/config.json" "$RESOURCES_PATH/"
    log "cp $SHARED_PATH/searchoptions.json $RESOURCES_PATH/"
    cp "$SHARED_PATH/searchoptions.json" "$RESOURCES_PATH/"

    # add to bin
    add_to_bin "$PLSEARCH_PATH/bin/plsearch.sh"
}

build_php () {
    echo
    hdr "build_php"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        echo "You need to install PHP"
        return
    fi

    # TODO: do a real version check
    if [ -z "$(php -v | grep 'cli')" ]
    then
        echo "A version of PHP >= 7.x is required"
        return
    fi

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        echo "Need to install composer"
        return
    fi

    CONFIG_PATH="$PHPSEARCH_PATH/config"
    RESOURCES_PATH="$PHPSEARCH_PATH/resources"

    # copy the shared config json file to the local config location
    mkdir -p "$CONFIG_PATH"
    log "cp $SHARED_PATH/config.json $CONFIG_PATH/"
    cp "$SHARED_PATH/config.json" "$CONFIG_PATH/"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    log "cp $SHARED_PATH/searchoptions.json $RESOURCES_PATH/"
    cp "$SHARED_PATH/searchoptions.json" "$RESOURCES_PATH/"

    cd "$PHPSEARCH_PATH/"

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
        return
    fi

    # add to bin
    add_to_bin "$PHPSEARCH_PATH/bin/phpsearch.sh"

    cd -
}

build_python () {
    echo
    hdr "build_python"

    # ensure python3.7+ is installed
    PYTHON_VERSIONS=(python3.11 python3.10 python3.9 python3.8 python3.7)
    PYTHON=
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
        log "A version of python >= 3.7 is required"
        return
    else
        PYTHON=$(basename "$PYTHON")
        log "Using $PYTHON ($(which $PYTHON))"
    fi

    # Set to Yes to use venv
    USE_VENV=$VENV

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$PYSEARCH_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    cd "$PYSEARCH_PATH"

    if [ "$USE_VENV" == 'yes' ]
    then
        log "Using venv"

        # if venv is active, deactivate it (in case it happens to be another venv that is active)
        if [ -n "$VIRTUAL_ENV" ]
        then
            deactivate
        fi

        # create a virtual env to run from and install to if it doesn't already exist
        if [ ! -d "$PYSEARCH_PATH/venv" ]
        then
            log "$PYTHON -m venv venv"
            "$PYTHON" -m venv venv
        fi

        # if venv isn't active, activate it
        # (TODO: this is probably always true because of earlier deactivation)
        if [ -z "$VIRTUAL_ENV" ]
        then
            log "source $PYSEARCH_PATH/venv/bin/activate"
            source $PYSEARCH_PATH/venv/bin/activate
        fi

        # get the path to the venv version
        PYTHON=$(which python3)
        PYTHON=$(basename "$PYTHON")
        log "Using $PYTHON ($(which $PYTHON))"
    fi

    # install wheel - this seems to fix problems with installing local dependencies,
    # which pyfind will be for pysearch
    log "pip3 install wheel"
    pip3 install wheel

    # install dependencies in requirements.txt
    log "pip3 install -r requirements.txt"
    pip3 install -r requirements.txt

    if [ "$USE_VENV" == 'yes' ]
    then
        # deactivate at end of setup process
        log "deactivate"
        deactivate
    fi

    # TODO: change the !# line in pysearch to use the determined python version

    # add to bin
    add_to_bin "$PYSEARCH_PATH/bin/pysearch.sh"

    cd -
}

build_ruby () {
    echo
    hdr "build_ruby"

    # ensure ruby2.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        echo "You need to install ruby"
        return
    fi

    # TODO: do a real version check (first determine minimum needed version)
    if [ -z "$(ruby -v | grep 'ruby 2')" ]
    then
        echo "A version of ruby >= 2.x is required"
        return
    fi

    # if [ -z "$(which bundle)" ]
    # then
    #     echo "You need to install bundler: https://bundler.io/"
    #     return
    # fi

    RESOURCES_PATH="$RBSEARCH_PATH/data"
    TEST_RESOURCES_PATH="$RBSEARCH_PATH/lib/test/fixtures"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # TODO: figure out how to install dependencies without installing rbsearch (which is what bundler does)
    # cd "$RBSEARCH_PATH"
    # log "bundle"
    # bundle
    # cd -

    # add to bin
    add_to_bin "$RBSEARCH_PATH/bin/rbsearch.sh"
}

build_rust () {
    echo
    hdr "build_rust"

    # ensure cargo/rust is installed
    if [ -z "$(which cargo)" ]
    then
        echo "You need to install rust"
        return
    fi

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

build_scala () {
    echo
    hdr "build_scala"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        echo "You need to install scala + sbt"
        return
    fi

    RESOURCES_PATH="$SCALASEARCH_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$SCALASEARCH_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_searchoptions_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

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
        return
    fi

    # add to bin
    add_to_bin "$SCALASEARCH_PATH/bin/scalasearch.sh"

    cd -
}

build_swift () {
    echo
    hdr "build_swift"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        echo "You need to install swift"
        return
    fi

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

build_typescript () {
    echo
    hdr "build_typescript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install node.js/npm"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$TSSEARCH_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

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
        return
    fi

    # add to bin
    add_to_bin "$TSSEARCH_PATH/bin/tssearch.sh"

    cd -
}

# build_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - build_clojure    - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - build_cpp        - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - build_go         - go is known for having very fast builds, and it's true, the only builds that are faster here
#                      are the ones that do nothing except copy over resources files (e.g. perl)
# - build_haskell    - having some dependency issues that need to work through to get it buildling again
# - build_javascript - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                      in an external terminal fixes the problem
# - build_kotlin     - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                      time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                      in a while
# - build_objc       - not sure if it's even possible to build this on linux, but excluding for now
# - build_ocaml      - had a number of different issues trying to get this version building again, finally
#                      gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - build_rust       - the first time this build is run it will pretty time-consuming, particularly for release
#                      target, but intermittent builds should be pretty fast
# - build_scala      - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - build_typescript - this build has the same problem as build_javascript; run the build in an external terminal
build_linux () {
    hdr "build_linux"

    # time build_c

    # time build_clojure

    # time build_cpp

    time build_csharp

    time build_dart

    time build_fsharp

    time build_go

    time build_java

    time build_javascript

    # time build_kotlin

    time build_perl

    time build_php

    time build_python

    time build_ruby

    time build_rust

    # time build_scala

    time build_swift

    time build_typescript
}

build_all () {
    hdr "build_all"

    # time build_c

    time build_clojure

    time build_cpp

    time build_csharp

    time build_dart

    time build_fsharp

    time build_go

    time build_haskell

    time build_java

    time build_javascript

    time build_kotlin

    time build_objc

    # time build_ocaml

    time build_perl

    time build_php

    time build_python

    time build_ruby

    time build_rust

    time build_scala

    time build_swift

    time build_typescript
}


########################################
# Build Main
########################################
HELP=
DEBUG=
RELEASE=
VENV=
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
        --debug)
            DEBUG=yes
            ;;
        --release)
            RELEASE=yes
            ;;
        --venv)
            VENV=yes
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

if [ -z "$DEBUG" ] && [ -z "$RELEASE" ]
then
    DEBUG=yes
fi

case $ARG in
    all)
        build_all
        ;;
    linux)
        build_linux
        ;;
    # c)
    #     build_c
    #     ;;
    clj | clojure)
        build_clojure
        ;;
    cpp)
        build_cpp
        ;;
    cs | csharp)
        build_csharp
        ;;
    dart)
        build_dart
        ;;
    fs | fsharp)
        build_fsharp
        ;;
    go)
        build_go
        ;;
    haskell | hs)
        build_haskell
        ;;
    java)
        build_java
        ;;
    javascript | js)
        build_javascript
        ;;
    kotlin | kt)
        build_kotlin
        ;;
    objc)
        build_objc
        ;;
    # ocaml | ml)
    #     build_ocaml
    #     ;;
    perl | pl)
        build_perl
        ;;
    php)
        build_php
        ;;
    # ps1 | powershell)
    #     build_powershell
    #     ;;
    py | python)
        build_python
        ;;
    rb | ruby)
        build_ruby
        ;;
    rs | rust)
        build_rust
        ;;
    scala)
        build_scala
        ;;
    swift)
        build_swift
        ;;
    ts | typescript)
        build_typescript
        ;;
    *)
        echo -n "ERROR: unknown xsearch build argument: $ARG"
        ;;
esac
