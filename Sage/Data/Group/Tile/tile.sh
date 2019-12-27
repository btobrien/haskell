#!/bin/bash

PATH=~/.config/$(basename $0):$PATH

set -o pipefail
echo
move $1 | coat show
