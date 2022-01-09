#!/bin/bash

mover=$1
map=$2

$mover </dev/null | $map | grep . | sed 's/./ /g' >cleaner
clean_height=$(wc -l <cleaner)
width=$(head -1 <cleaner | wc -m)
width=$((width/2))
height=$clean_height

>moves
tail -f moves 2>/dev/null | $mover >positions &
sleep '.1'
tail -1 <positions | $map | grep . 
tput cuu $height
x=0
y=0

tput cnorm
tail -1 <positions | tr -d '[' | tr -d ']' | tr ',' '\n' >edges

stty -echo
trap "kill $!; tput cud $((height - 1)); stty echo" EXIT 

function move 
{
    tput civis
    (( $x == '0' )) || tput cub $((2 * x))
    (( $y == '0' )) || tput cuu $y
	before=$(wc -l <positions)
	echo "$1 ($x,$y)" >>moves
	after=$(wc -l <positions)
	while (( before == after )); do
		after=$(wc -l <positions)
		sleep '.05'
	done
	cat cleaner
    tput cuu $clean_height
	tail -1 <positions | $map | grep . 
	height=$(tail -1 <positions | $map | grep . | wc -l)
    tput cuu $height
    (( $y == 0 )) || tput cud $y
    (( $x == 0 )) || tput cuf $((2 * x))
    tput cnorm
	tail -1 <positions | tr -d '[' | tr -d ']' | tr ',' '\n' >edges
}

function down
{
    tput cud1
    if (( $x != 0 )); then
        tput cuf $((2 * x))
        edge=$(head -n$x <edges | tail -n1)
        if (( $edge == $y )); then
            tput cuu1
        else
            ((y++))
        fi
    else
        ((y++))
    fi
}

function up
{
    tput cuu1
    ((y--))
}

function left
{
    tput cub 2
    ((x--))
}

function right
{
    tput cuf 2
    ((x++))
    edge=$(head -n$x <edges | tail -n1)
    if (( $edge < $y )); then
        tput cub 2
        ((x--))
    fi
}

while read -n1 char; do
    if [ "$char" == 'j' ]; then
        (( $y == $((height - 1)) )) || down
    elif [ "$char" == 'k' ]; then
        (( $y == '0' )) || up
    elif [ "$char" == 'h' ]; then
        (( $x == '0' )) || left
    elif [ "$char" == 'l' ]; then
        (( $x == $((width - 1)) )) || right
    elif [ "$char" == '' ]; then
        move 'z'
    elif [ "$char" == 'v' ]; then #response
        move 'n'
		sleep '0.5'
        move 'w'
    else
        move "$char"
    fi
done
