#!/bin/bash

source ~/.config/noise/import

encoder=$1
length=${2:-1}

check $length 0 "$encoder" | tr -d '\n'

e=0
while check $length $e "$encoder" >/dev/null; do
    ((++e))
done

echo " $((e - 1))"
