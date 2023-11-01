#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

OBJCSEARCH_PATH="$XSEARCH_PATH/objc/objcsearch"
OBJCSEARCH_EXE="$OBJCSEARCH_PATH/.build/debug/objcsearchApp"

$OBJCSEARCH_EXE "$@"
