#!/bin/bash

#title="set title \"$2\";"
gnuplot -e "set terminal dumb $(tput cols) $(tput lines); $title plot '<cat' notitle with $1"
