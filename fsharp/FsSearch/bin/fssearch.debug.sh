#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

FSSEARCH_PATH="$XSEARCH_PATH/fsharp/FsSearch"
CONFIGURATION=Debug
# CONFIGURATION=Release
DOTNET_VERSION=net9.0
FSSEARCH_EXE="$FSSEARCH_PATH/FsSearch/bin/$CONFIGURATION/$DOTNET_VERSION/FsSearch"

$FSSEARCH_EXE "$@"
