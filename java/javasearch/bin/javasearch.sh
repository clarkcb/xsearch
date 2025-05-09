#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

JAVAFIND_PATH="$XFIND_PATH/java/javafind"
JAVAFIND_VERSION="0.1.0-SNAPSHOT"
JAVAFIND_JAR="$JAVAFIND_PATH/build/libs/javafind-$JAVAFIND_VERSION.jar"

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

JAVASEARCH_PATH="$XSEARCH_PATH/java/javasearch"
# JAVASEARCH_VERSION="0.1.0-SNAPSHOT"
JAVASEARCH_LIB_JAR="$JAVASEARCH_PATH/lib/build/libs/lib.jar"
JAVASEARCH_APP_JAR="$JAVASEARCH_PATH/app/build/libs/app.jar"
JAVASEARCH_CLASSPATH="$JAVAFIND_JAR:$JAVASEARCH_LIB_JAR:$JAVASEARCH_APP_JAR"

# JAVA_HOME="/usr/local/Cellar/openjdk/23.0.1/libexec/openjdk.jdk/Contents/Home"
JAVA_HOME=$(/usr/libexec/java_home -v17)

java -cp "$JAVASEARCH_CLASSPATH" javasearch.JavaSearch "$@"
