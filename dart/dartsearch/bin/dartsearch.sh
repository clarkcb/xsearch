#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

DARTSEARCH_PATH=$XSEARCH_PATH/dart/dartsearch
DARTSEARCH_EXE=$DARTSEARCH_PATH/bin/dartsearch.exe

"$DARTSEARCH_EXE" "$@"
