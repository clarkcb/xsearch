#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

SCALASEARCH_PATH="$XSEARCH_PATH/scala/scalasearch"
SCALA_VERSION="3.7.4"
SCALAFIND_VERSION="0.1.0"
SCALASEARCH_JAR="$SCALASEARCH_PATH/target/scala-$SCALA_VERSION/scalasearch-assembly-$SCALAFIND_VERSION.jar"

# echo "java -Xms1G -Xmx2G -cp $SCALASEARCH_JAR scalasearch.SearchMain $@"
java -cp "$SCALASEARCH_JAR" "scalasearch.SearchMain" "$@"
