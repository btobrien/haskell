#!/bin/bash

cd ~/.config/wpm

if [ "$1" == 'wordle' ]; then
	shift
	numwords=${1:-30}
	shuf wordleWords.txt | head -n$numwords | fmt >text
elif ! cp $1 text 2>/dev/null; then
	numwords=${1:-30}
	shuf 1000words.txt | head -n$numwords | fmt >text
fi
cat text

textlen=$(wc -l <"text")
tput cuu $textlen

stty -echo
old_IFS=$IFS; IFS=""
trap "tput cud $textlen; tput sgr0; stty echo; IFS=$old_IFS" EXIT

tput setaf 2 # set green
white='\033[0m'; red='\033[0;31m'; green='\033[0;32m'

function wrong {
	if [ "$key" == " " ]; then
		printf "${red}_${green}"
    else
		printf "${red}${key}${green}"
	fi
	((x++))
}

function back {
    if (($x != 0)); then
		((x--))
		next=${line:x:1}
		printf "\b${white}${next}${green}\b"
    elif (($y != 1)); then
		tput cuu1
		((y--))
		line=$(head -n$y <text | tail -1)
		linelen=$(wc -c <<<"$line")
		((linelen--))
		tput cuf $linelen
		x=$linelen
	fi
}

function newline {
	echo
	x=0
	((y++))
	line=$(head -n$y <text | tail -1)
	linelen=$(wc -c <<<"$line")
	((linelen--))
}

x=0
y=1

line=$(head -1 <text)
linelen=$(wc -c <<<"$line")
((linelen--))

words=$(wc -w <text)
words60=$((60 * words))

function timestamp {
    perl -MTime::HiRes=time -e 'printf "%.3f\n", time'
}
# date +%s.%3N not supported on macos

start=$(timestamp)

while read -n1 key; do
	next=${line:x:1}
    key=$(cat -v <<<$key)
	if (($x == $linelen)) && [ "$key" != '^?' ] && [ "$key" != '^H' ]; then
        if (($y == $textlen)); then
            finish=$(timestamp)
			echo
			break
		fi
		newline
	elif [ "$key" == "$next" ]; then
		printf "$key"; ((x++))
	elif [ "$key" == '^?' ] || [ "$key" == '^H' ]; then
		back
	else
		wrong
	fi
done

seconds=$(bc <<<"scale=3; $finish - $start")
wpm=$(bc <<<"scale=1; $words60/$seconds")
tput setaf 3 # set yellow
echo $wpm
echo
trap "tput sgr0; stty echo; IFS=$old_IFS" EXIT  # also want to clear old trap, no need to move cursor down

