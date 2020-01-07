#!/bin/bash

config=$HOME/.config/hanoi
PATH=$config/bin/:$PATH

set -o pipefail
move $@ | coat show
