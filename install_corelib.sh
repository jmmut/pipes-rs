#!/bin/bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
#echo $SCRIPT_DIR

INSTALL_DIR=~/.local/share/pipes/
mkdir -p  $INSTALL_DIR

rm -f ${INSTALL_DIR}/core

ln -s "${SCRIPT_DIR}/pipes_programs/corelib" ${INSTALL_DIR}/core


