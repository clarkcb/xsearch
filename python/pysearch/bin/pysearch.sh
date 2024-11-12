#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

PYSEARCH_PATH="$XSEARCH_PATH/python/pysearch"

PYSEARCH_EXE="$PYSEARCH_PATH/bin/pysearch.py"

python3 "$PYSEARCH_EXE" "$@"
