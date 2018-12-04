#!/bin/sh

XSEARCH_PATH=$HOME/src/xsearch
OBJC_PATH=$XSEARCH_PATH/objc
OBJCSEARCH_PATH=$OBJC_PATH/objcsearch

# Debug exe location (when compiling from within Xcode)
#OBJCSEARCHEXE=$HOME/Library/Developer/Xcode/DerivedData/objcsearch-ahhnhqcmbhdevtgqfhmgnnerqaln/Build/Products/Debug/objcsearch

# Release exe location (when compiling from command line (scripts/build.sh objc))
OBJCSEARCHEXE=$OBJCSEARCH_PATH/build/Release/objcsearch

$OBJCSEARCHEXE $@
