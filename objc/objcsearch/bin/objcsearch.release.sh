#!/bin/sh

if [ -z "$XSEARCH_PATH" ]
then
    XSEARCH_PATH=$HOME/src/xsearch
fi

OBJCSEARCH_PATH=$XSEARCH_PATH/objc/objcsearch

# Debug exe location (when compiling from within Xcode)
#OBJCSEARCHEXE=$HOME/Library/Developer/Xcode/DerivedData/objcsearch-ahhnhqcmbhdevtgqfhmgnnerqaln/Build/Products/Debug/objcsearch

# Release exe location (when compiling from command line (scripts/build.sh objc))
OBJCSEARCH_EXE=$OBJCSEARCH_PATH/build/Release/objcsearch

$OBJCSEARCH_EXE $@
