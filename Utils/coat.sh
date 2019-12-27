#!/bin/bash

painter=$1

tput civis
trap "tput cnorm" EXIT

read line
$painter <<< "$line"
height=$($painter <<< "$line" | wc -l)

while read line; do
    tput cuu $height
    tput el1
    $painter <<< "$line"
done
