#!/bin/sh
XSEARCH_PATH=~/src/xsearch
FSSEARCH_PATH=$XSEARCH_PATH/fsharp
CONFIGURATION=Debug
#CONFIGURATION=Release
FSSEARCHEXE=$FSSEARCH_PATH/FsSearch/bin/$CONFIGURATION/FsSearch.exe
mono $FSSEARCHEXE $@
