#!/bin/bash

if [ $1 == 'big' ]; then
	if ! [ "$3" == '' ]; then
		title="set title \"$3\";"
	fi
	gnuplot -e "set terminal dumb $(tput cols) $(tput lines); $title plot '<cat' notitle with $2"
	exit
fi

if ! [ "$2" == '' ]; then
	title="set title \"$2\";"
fi

gnuplot -e "set terminal dumb; $title plot '<cat' notitle with $1"
