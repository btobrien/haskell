#!/bin/bash

cd ~/.config/sierpinski

if [ "$1" == '' ] || grep '\.' <<<$1 >/dev/null; then
    throttle=${1:-'0.5'}
    shift
fi

size=${1:-5}
i=0

stty -echo 2>/dev/null
tput civis

length=$(./sierpinski $size 0 | wc -l)
trap "clear; tput cnorm; tput setaf 7; stty echo" EXIT 

function iterate {
    i=$((i % (size + 1)))
    ((i < 0)) && i=$size
    tput setaf $((1 + i))
    ./sierpinski $size $i | center
    tput cuu $length
}

tput setaf 1
./sierpinski $size 0 | center
tput cuu $length

if [ "$throttle" != '' ]; then
    while true; do
        sleep $throttle
        ((i++))
        iterate
    done
    exit
fi

while read -n1 key; do
    [ "$key" == 'j' ] && ((i++))
    [ "$key" == 'k' ] && ((i--))
    iterate
done
