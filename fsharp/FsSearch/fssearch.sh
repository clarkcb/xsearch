#!/bin/bash

SCRIPTPATH=$( readlink "${BASH_SOURCE[0]}" )
SCRIPTDIR=$( dirname "$SCRIPTPATH" )
source "$SCRIPTDIR/../../scripts/config.sh" 

FSSEARCH_PATH=$FSHARP_PATH/FsSearch
CONFIGURATION=Debug
#CONFIGURATION=Release
FSSEARCHEXE=$FSSEARCH_PATH/FsSearch/bin/$CONFIGURATION/netcoreapp2.2/FsSearch.dll

dotnet $FSSEARCHEXE $@
