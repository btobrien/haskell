#!/bin/bash

# add functionality to show only used/unused words..
if [ "$1" == 'all' ]; then
	shuf <~/.config/wordle/guesses.txt
else
	shuf <~/.config/wordle/words.txt
fi

