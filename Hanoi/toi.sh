#!/bin/bash

config=$HOME/.config/hanoi
PATH=$config:$PATH

set -o pipefail
move $@ | coat show
