#!/bin/bash

#clear
painter=$1

tput civis
trap "tput cnorm" EXIT

tput sc
while read line; do
    tput rc
    tput sc
    $painter <<<"$line"
done
