#!/bin/sh

SCRIPTPATH=$( readlink "${BASH_SOURCE[0]}" )
SCRIPTDIR=$( dirname "$SCRIPTPATH" )
source "$SCRIPTDIR/../../scripts/config.sh" 

CSSEARCH_PATH=$CSHARP_PATH/CsSearch
CONFIGURATION=Debug
#CONFIGURATION=Release
CSSEARCHEXE=$CSSEARCH_PATH/CsSearch/bin/$CONFIGURATION/CsSearch.exe

mono $CSSEARCHEXE $@
