#!/bin/sh
################################################################################
#
# lint.sh
#
# Run static code analysis tools
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"


########################################
# Lint Functions
########################################

lint_clojure () {
    echo
    hdr "lint_clojure"

    cd $CLJSEARCH_PATH

    log "Linting cljsearch"
    log "lein eastwood"
    lein eastwood

    cd -
}

lint_cpp () {
    echo
    hdr "lint_cpp"

    log "not implemented at this time"
}

lint_csharp () {
    echo
    hdr "lint_csharp"

    log "not implemented at this time"
}

lint_dart () {
    echo
    hdr "lint_dart"

    log "Linting dartsearch"
    log "dart analyze $DARTSEARCH_PATH"
    dart analyze $DARTSEARCH_PATH
}

lint_fsharp () {
    echo
    hdr "lint_fsharp"

    log "not implemented at this time"
}

lint_go () {
    echo
    hdr "lint_go"

    cd $GOSEARCH_PATH

    log "Linting gosearch"
    log "go vet ./..."
    go vet ./...

    cd -
}

lint_haskell () {
    echo
    hdr "lint_haskell"

    HLINT=$HOME/.local/bin/hlint

    log "Linting hssearch"
    log "hlint $HSSEARCH_PATH"
    $HLINT $HSSEARCH_PATH
}

lint_java () {
    echo
    hdr "lint_java"

    TOOLS_PATH=$JAVA_PATH/tools
    if [ ! -d "$TOOLS_PATH" ]
    then
        log "mkdir -p $TOOLS_PATH"
        mkdir -p $TOOLS_PATH
    fi

    CHECKSTYLE_JAR=$(find $TOOLS_PATH -name "checkstyle*.jar" | head -n 1)
    if [ -z "$CHECKSTYLE_JAR" ]
    then
        log "Checkstyle jar not found, downloading"
        URL="https://github.com/checkstyle/checkstyle/releases/download/checkstyle-8.41/checkstyle-8.41-all.jar"
        cd $TOOLS_PATH
        curl -J -L -O $URL
        cd -
        CHECKSTYLE_JAR=$(find $TOOLS_PATH -name "checkstyle*.jar" | head -n 1)
    fi

    JAVA=$JAVA_HOME/bin/java
    CONFIG=$JAVASEARCH_PATH/sun_checks.xml
    #CONFIG=$JAVASEARCH_PATH/google_checks.xml

    GREPVS=("Javadoc"
            "hides a field"
            "Line is longer than 80 characters"
            "Missing a Javadoc comment"
            "Missing package-info.java file"
            )

    log "Linting javasearch"
    FILES=$(find $JAVASEARCH_PATH/src -name "*.java")
    for f in ${FILES[*]}
    do
        echo
        log "java -jar $CHECKSTYLE_JAR -c $CONFIG $f"
        output=$($JAVA -jar $CHECKSTYLE -c $CONFIG $f)
        # for g in ${GREPVS[*]}
        # do
        #     output=$(echo $output | grep -v $g)
        # done
        echo -e $output
    done
}

lint_javascript () {
    echo
    hdr "lint_javascript"

    JSSRC_PATH=$JSSEARCH_PATH/src
    JSHINT=$JSSEARCH_PATH/node_modules/jshint/bin/jshint

    if [ ! -f $JSHINT ]
    then
        cd $JSSEARCH_PATH
        npm install jshint
        cd -
    fi

    log "Linting jssearch"
    FILES=$(find $JSSRC_PATH -name "*.js")
    for f in ${FILES[*]}
    do
        log "$JSHINT $f"
        $JSHINT $f
    done
}

lint_kotlin () {
    echo
    hdr "lint_kotlin"

    if [ -z "$(which ktlint)" ]
    then
        echo "You need to install ktlint"
        return
    fi

    log "Linting ktsearch"
    cd $KOTLIN_PATH
    log "ktlint"
    ktlint
    cd -
}

lint_objc () {
    echo
    hdr "lint_objc"

    log "not implemented at this time"
}

lint_ocaml () {
    echo
    hdr "lint_ocaml"

    log "not implemented at this time"
}

lint_perl () {
    echo
    hdr "lint_perl"

    log "not implemented at this time"
}

lint_php () {
    echo
    hdr "lint_php"
    #PHP_PATH=$XSEARCH_PATH/php
    #PHPSEARCH_PATH=$PHP_PATH/phpsearch
    #PHPLINT=$PHP_PATH/tools/phplint-2.0_20141127/phpl
    ## Analyze
    #log "Linting phpsearch.php"
    #FILES=$(find $PHPSEARCH_PATH -name "*.php")
    #for f in ${FILES[*]}; do
    #    echo "$PHPLINT $f"
    #    $PHPLINT $f
    #done
    log "not implemented at this time"
}

lint_python () {
    echo
    hdr "lint_python"

    log "Linting pysearch"
    cd $PYSEARCH_PATH
    log "pylint pysearch"
    pylint pysearch
    cd -
}

lint_ruby () {
    echo
    hdr "lint_ruby"

    if [ -z "$(which ruby-lint)" ]
    then
        echo "You need to install ruby-lint"
        return
    fi

    log "Linting rbsearch"
    FILES=$(find $RBSEARCH_PATH -name "*.rb")
    for f in ${FILES[*]}
    do
        log "ruby-lint $f"
        ruby-lint $f | grep -v 'undefined'
    done
}

lint_rust () {
    echo
    hdr "lint_rust"

    log "not implemented at this time"
}

lint_scala () {
    echo
    hdr "lint_scala"

    TOOLS_PATH=$SCALA_PATH/tools
    if [ ! -d "$TOOLS_PATH" ]
    then
        log "mkdir -p $TOOLS_PATH"
        mkdir -p $TOOLS_PATH
    fi

    SCALASTYLE_JAR=$(find $TOOLS_PATH -name "scalastyle*.jar" | head -n1)
    if [ -z "$SCALASTYLE_JAR" ]
    then
        log "Scalastyle jar not found, downloading"
        # URL="https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_2.11/0.7.0/scalastyle_2.11-0.7.0-batch.jar"
        URL="https://repo1.maven.org/maven2/org/scalastyle/scalastyle_2.12/1.0.0/scalastyle_2.12-1.0.0.jar"
        cd $TOOLS_PATH
        curl -O $URL
        cd -
        SCALASTYLE_JAR=$(find $TOOLS_PATH -name "scalastyle*.jar" | head -n1)
    fi

    CONFIG=$SCALASEARCH_PATH/scalastyle_config.xml

    log "Linting scalasearch"
    log "java -jar $SCALASTYLE_JAR --config $CONFIG $SCALASEARCH_PATH/src/main/scala"
    java -jar $SCALASTYLE_JAR --config $CONFIG $SCALASEARCH_PATH/src/main/scala
}

lint_swift () {
    # Assumes that swiftlint has been installed, do this on OSX:
    # $ sudo brew install swiftlint
    echo
    hdr "lint_swift"

    if [ -z "$(which swiftlint)" ]
    then
        echo "You need to install swiftlint"
        return
    fi

    log "Linting swiftsearch"
    log "cd $SWIFTSEARCH_PATH; swiftlint; cd -"
    cd $SWIFTSEARCH_PATH; swiftlint; cd -
}

lint_typescript () {
    echo
    hdr "lint_typescript"

    log "Not supported at this time"
}

lint_all () {
    log "lint_all"
    
    lint_clojure

    lint_cpp

    lint_csharp

    lint_dart

    lint_fsharp

    lint_go

    lint_haskell

    lint_java

    lint_javascript

    lint_kotlin

    lint_objc

    lint_ocaml

    lint_perl

    lint_php

    lint_python

    lint_ruby

    lint_rust

    lint_scala

    lint_swift

    lint_typescript
}


########################################
# Lint Steps
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    lint_all
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    lint_clojure
elif [ "$ARG" == "cpp" ]
then
    lint_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    lint_csharp
elif [ "$ARG" == "dart" ]
then
    lint_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    lint_fsharp
elif [ "$ARG" == "go" ]
then
    lint_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    lint_haskell
elif [ "$ARG" == "java" ]
then
    lint_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    lint_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    lint_kotlin
elif [ "$ARG" == "objc" ]
then
    lint_objc
elif [ "$ARG" == "ocaml" ] || [ "$ARG" == "ml" ]
then
    lint_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    lint_perl
elif [ "$ARG" == "php" ]
then
    lint_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    lint_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    lint_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    lint_rust
elif [ "$ARG" == "scala" ]
then
    lint_scala
elif [ "$ARG" == "swift" ]
then
    lint_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    lint_typescript
else
    echo "ERROR: unknown lint argument: $ARG"
fi

