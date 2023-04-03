#!/bin/bash
################################################################################
#
# pyvenv_setup.sh
#
# Sets up a venv for python under current directory
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"
VENV_PATH=$DIR/venv
PYTHON_EXE=python3.9
PIP_EXE=pip3.9
REQUIREMENTS_PATH=$PYSEARCH_PATH/requirements.txt

echo "DIR: $DIR"
echo "VENV_PATH: $VENV_PATH"


########################################
# Build Functions
########################################

create_activate_venv () {
    echo
    hdr "create_activate_venv"

    # if venv doesn't exist, create it
    if [ ! -d "$VENV_PATH" ]
    then
        log "$VENV_PATH not found, creating..."

        log "$PYTHON_EXE -m venv venv"
        "$PYTHON_EXE" -m venv venv
    fi

    # if venv isn't active, activate it
    if [ -z "$VIRTUAL_ENV" ]
    then
        log "source ./venv/bin/activate"
        source ./venv/bin/activate

        # try to upgrade pip
        log "$PYTHON_EXE -m pip install --upgrade pip"
        "$PYTHON_EXE" -m pip install --upgrade pip
    fi

    # install pyfind's dependencies
    log "$PIP_EXE install -r $REQUIREMENTS_PATH"
    "$PIP_EXE" install -r "$REQUIREMENTS_PATH"

    # if a local requirements.txt exists, install its dependencies as well
    if [ -f ./requirements.txt ]
    then
        log "$PIP_EXE install -r ./requirements.txt"
        "$PIP_EXE" install -r ./requirements.txt
    fi
}


########################################
# Main
########################################

create_activate_venv
