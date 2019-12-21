#!/bin/bash

target=${1:-"install"}
dir=${2:-"$HOME"}

find "$dir" -d -name 'top' -exec bash -c "cd '{}' && pwd && make $target" \;
exit 0
