#!/bin/bash

PATH=~/.config/$(basename $0):$PATH

if [ "$1" == 'clone' ]; then
    clone $(basename $0)
fi

set -o pipefail
echo
move $1 | coat show
