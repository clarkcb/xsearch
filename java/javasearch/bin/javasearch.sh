#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

JAVASEARCH_PATH="$XSEARCH_PATH/java/javasearch"
JAVASEARCH_VERSION="0.1.0-SNAPSHOT"
# JAVASEARCH_JAR=$(find "$JAVASEARCH_PATH/target" -name "javasearch*.jar" | head -n 1)
JAVASEARCH_JAR="$JAVASEARCH_PATH/target/javafind-$JAVASEARCH_VERSION.jar"

java -cp "$JAVASEARCH_JAR" javasearch.SearchMain "$@"
