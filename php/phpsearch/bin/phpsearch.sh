#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH="$HOME/src/xsearch"
fi

PHPSEARCH_PATH="$XSEARCH_PATH/php/phpsearch"

PHPSEARCH_EXE="$PHPSEARCH_PATH/bin/phpsearch.php"

php "$PHPSEARCH_EXE" "$@"
