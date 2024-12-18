#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

SCALASEARCH_PATH="$XSEARCH_PATH/scala/scalasearch"
SCALA_VERSION=3.6.2
SCALASEARCH_JAR=$(find "$SCALASEARCH_PATH/target/scala-$SCALA_VERSION" -maxdepth 1 -name "scalasearch-assembly-*.jar" | head -n 1)

# echo "java -Xms1G -Xmx2G -cp $SCALASEARCH_JAR scalasearch.SearchMain $@"
java -cp "$SCALASEARCH_JAR" "scalasearch.SearchMain" "$@"
