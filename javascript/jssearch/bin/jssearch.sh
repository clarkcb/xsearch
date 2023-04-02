#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

JSSEARCH_PATH=$XSEARCH_PATH/javascript/jssearch

node $JSSEARCH_PATH/dist/jssearch.js "$@"
