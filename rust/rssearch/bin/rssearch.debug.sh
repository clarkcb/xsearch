#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

RSSEARCH_PATH=$XSEARCH_PATH/rust/rssearch
PROFILE=debug
# PROFILE=release

$RSSEARCH_PATH/target/$PROFILE/rssearch "$@"
