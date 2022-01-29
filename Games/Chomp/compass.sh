#!/bin/bash

source ~/.config/chomp/import.sh

mover="move $1"
map="show"

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
trap "kill $(jobs -p); tput cud $((height - 1)); stty echo" EXIT 

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
		sleep '.05' # better option than busy-waiting?
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
	width=$(tail -1 <positions | $map | head -n1 | wc -m)
    width=$((width/2))
}

function down
{
    tput cud1
    ((y++))
    if (( $x != 0 )); then
        tput cuf $((2 * x))
        edge=$(head -n$x <edges | tail -n1)
        if (( $edge < $y )); then
            tput cuu1
            ((y--))
        fi
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
        move 'M' # test winner
    elif [ "$char" == 'u' ]; then
        move "-$((RANDOM % 100))" # just random
    elif [ "$char" == 'v' ]; then
        move 'm'  # move
		sleep '0.5'
        move 'w'  # winner
    elif [ "$char" == 'n' ]; then
        move 'm'  # move
		sleep '0.5'
        move $((RANDOM % 100)) # win/random
    elif [ "$char" == 'N' ]; then
        move $((RANDOM % 100))
    else
        move "$char"
    fi
done
