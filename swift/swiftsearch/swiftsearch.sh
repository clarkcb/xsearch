#!/bin/sh

XSEARCH_PATH=$HOME/src/xsearch
SWIFT_PATH=$XSEARCH_PATH/swift
SWIFTSEARCH_PATH=$SWIFT_PATH/swiftsearch

# Debug exe location (when compiling from within Xcode)
#SWIFTSEARCHEXE=$HOME/Library/Developer/Xcode/DerivedData/swiftsearch-fnxmcvhlrjapoaeeqzmwblpnnffc/Build/Products/Debug/swiftsearch

# Release exe location (when compiling from command line (scripts/build.sh swift))
SWIFTSEARCHEXE=$SWIFTSEARCH_PATH/build/Release/swiftsearch

$SWIFTSEARCHEXE $@
