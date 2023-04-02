#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

JAVASEARCH_PATH=$XSEARCH_PATH/java/javasearch
JAVASEARCH_JAR=$(find $JAVASEARCH_PATH/target -name "javasearch*.jar" | head -n 1)
#echo $JAVASEARCH_JAR

java -cp $JAVASEARCH_JAR javasearch.SearchMain $@
