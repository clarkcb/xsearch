#!/bin/bash

RSSEARCH_PATH=/Users/cary/src/xsearch/rust/rssearch
PROFILE=debug
#PROFILE=release

$RSSEARCH_PATH/target/$PROFILE/rssearch "$@"
