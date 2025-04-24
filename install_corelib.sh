#!/bin/bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
echo $SCRIPT_DIR

mkdir -p  ~/.local/share/pipes/

ln -sfT "${SCRIPT_DIR}/pipes_programs/corelib" ~/.local/share/pipes/core


