#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

RBSEARCH_PATH="$XSEARCH_PATH/ruby/rbsearch"

RBSEARCH_EXE="$RBSEARCH_PATH/bin/rbsearch.rb"

ruby "$RBSEARCH_EXE" "$@"
