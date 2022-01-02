#!/bin/bash

map=$1
size=$($map </dev/null | wc -l)
echo | tr -d '\n' >moves
$map </dev/null
tput cuu $size

x=0
y=0

stty -echo
trap "tput cud $((size - 1)); stty echo" EXIT

function move 
{
    tput civis
    [ $x == '0' ] || tput cub $(( 2 * x ))
    [ $y == '0' ] || tput cuu $y
    echo "$1 $(($x + $size * $y))" >>moves
    $map <moves
    tput cuu $size
    [ $y == '0' ] || tput cud $y
    [ $x == '0' ] || tput cuf $(( 2 * x ))
    tput cnorm
}

function down
{
    tput cud1
    [ $x == '0' ] || tput cuf $(( 2 * x ))
    (( y++ ))
}

function up
{
    tput cuu1
    (( y-- ))
}

function left
{
    tput cub 2
    (( x-- ))
}

function right
{
    tput cuf 2
    (( x++ ))
}

while read -n1 char; do
    if [ "$char" == 'j' ]; then
        [ $y == $(( size - 1 )) ] || down
    elif [ "$char" == 'k' ]; then
        [ $y == '0' ] || up
    elif [ "$char" == 'h' ]; then
        [ $x == '0' ] || left
    elif [ "$char" == 'l' ]; then
        [ $x == $(( size - 1 )) ] || right
    else
        move "$char"
    fi
done
