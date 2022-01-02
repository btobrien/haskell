#!/bin/bash
export PATH="~/.config/chomp/bin":"$PATH"

./move $1 | ./show
