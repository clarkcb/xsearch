#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

TSSEARCH_PATH=$XSEARCH_PATH/typescript/tssearch

node $TSSEARCH_PATH/dist/tssearch.js "$@"
