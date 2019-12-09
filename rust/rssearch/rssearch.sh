#!/bin/bash

OLDPWD=`pwd`
RSSEARCH_PATH=/Users/cary/src/xsearch/rust/rssearch
PROFILE=debug
#PROFILE=release

cd $RSSEARCH_PATH
#cargo run -- $@
./target/$PROFILE/rssearch "$@"

cd $OLDPWD
