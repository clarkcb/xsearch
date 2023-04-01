#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

PLSEARCH_PATH=$XSEARCH_PATH/perl/plsearch

PLSEARCH_EXE=$PLSEARCH_PATH/bin/plsearch.pl

perl $PLSEARCH_EXE $@
