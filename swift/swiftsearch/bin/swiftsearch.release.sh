#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

SWIFTSEARCH_PATH=$XSEARCH_PATH/swift/swiftsearch
SWIFTSEARCH_EXE=$SWIFTSEARCH_PATH/.build/release/swiftsearchApp

$SWIFTSEARCH_EXE $@
