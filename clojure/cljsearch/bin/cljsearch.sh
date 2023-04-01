#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

CLJSEARCH_PATH=$XSEARCH_PATH/clojure/cljsearch
CLJSEARCH_JAR=$(find $CLJSEARCH_PATH/target/uberjar -name "cljsearch*.jar" | grep standalone | head -n 1)

java -jar "$CLJSEARCH_JAR" "$@"
