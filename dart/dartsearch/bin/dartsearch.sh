#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

DARTSEARCH_PATH=$XSEARCH_PATH/dart/dartsearch
PACKAGES_PATH=$DARTSEARCH_PATH/.packages
DARTSEARCH_EXE=$DARTSEARCH_PATH/bin/dartsearch.dart

# dart --packages="$PACKAGES_PATH" "$DARTSEARCH_EXE" "$@"
dart "$DARTSEARCH_EXE" "$@"
