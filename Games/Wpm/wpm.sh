#!/bin/bash

dir=~/.config/wpm
mkdir -p $dir

text=$dir/text.txt
if ! cp $1 $text 2>/dev/null; then
	numwords=$1
	shuf $dir/1000words.txt | head -n$numwords | fmt >$text
fi
cat $text

textlen=$(wc -l <"$text")
tput cuu $textlen

stty -echo
old_IFS=$IFS
IFS=""
trap "tput cud $textlen; tput sgr0; stty echo; IFS=$old_IFS" EXIT

backspace=$(cat << eof
0000000 005177
0000002
eof
)

tput setaf 2 # set green
white='\033[0m'
red='\033[0;31m'
green='\033[0;32m'

function wrong
{
	if [ "$key" == " " ]; then
		printf "${red}_${green}"
    else
		printf "${red}${key}${green}"
	fi
	(( x++ ))
}

function back
{
	if [ $x != '0' ]; then
		tput cub1
		(( x-- ))
		next=${line:x:1}
		printf "${white}${next}${green}"
		tput cub1
	elif [ $y != '1' ]; then
		tput cuu1
		(( y-- ))
		line=$(head -n$y <$text | tail -1)
		linelen=$(wc -c <<<"$line")
		(( linelen-- ))
		tput cuf $linelen
		x=$linelen
	fi
}

function newline 
{
	echo
	x=0
	(( y++ ))
	line=$(head -n$y <$text | tail -1)
	linelen=$(wc -c <<<"$line")
	(( linelen-- ))
}

x=0
y=1

line=$(head -1 <$text)
linelen=$(wc -c <<<"$line")
(( linelen-- ))

words=$(wc -w <$text)
words60=$(( 60 * words ))

start=$(date +%s.%3N)
while read -n1 key; do
	next=${line:x:1}
	if [ $x == $linelen ] && [[ $(od <<<"$key") != "$backspace" ]]; then
		if [ $y == $textlen ]; then
			finish=$(date +%s.%3N)
			echo
			break
		fi
		newline
	elif [ "$key" == "$next" ]; then
		printf "$key"; (( x++ ))
	elif [[ $(od <<<"$key") = "$backspace" ]]; then
		back
	else
		wrong
	fi
done

seconds=$(bc <<<"scale=3; $finish - ($start + 0.5)")
wpm=$(bc <<<"scale=1; $words60/$seconds")
tput setaf 3 # set yellow
echo $wpm
echo
trap "tput sgr0; stty echo; IFS=$old_IFS" EXIT  # also want to clear old trap, no need to move cursor down

