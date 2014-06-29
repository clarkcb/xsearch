#!/bin/sh
XSEARCH_PATH=/Users/cary/src/git/xsearch
FSSEARCH_PATH=$XSEARCH_PATH/fsharp
FSSEARCHEXE=$FSSEARCH_PATH/FsSearch/bin/Release/FsSearch.exe
mono $FSSEARCHEXE $@
