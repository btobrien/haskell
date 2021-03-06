#!/bin/bash

config=$HOME/.config/hanoi
export PATH=$config/bin:$PATH

mkdir -p $config/scores/

size=${1:-0}
player=$2

isbot=false
if ! [[ -t 0 ]]; then
    isbot=true
fi

if [ "$size" == '0' ]; then
	while true; do
		((size++))
		clear
		printf "\\e[1;30m"
		printf "level $size : "
		date
		printf "\\e[1;0m"
		echo
		soi $size | throttle | hanoi $size
	done
fi

if [ "$player" == 'solve' ]; then
    soi $size | hanoi $size bot
    exit 0
fi

if [ "$player" == 'solution' ]; then
    echo
    soi $size | sed 's/../& /g' | tr ' ' '\n' | hang
    echo
    exit 0
fi

/usr/bin/time toi $size 2>$config/last; [ "$?" == 0 ] || exit 4

[ "$player" != '' ] || exit 0

echo
score=$(awk '{ print $1 }' <$config/last)
echo $score

if [ "$player" == 'bot' ]; then
    exit 0
elif $isbot; then
    echo "automated moves detected: score not recorded"
    echo
else
    echo "$score $player" >>$config/scores/$size
fi

echo
echo 'leader board'
echo '------------'
sort -n $config/scores/$size 2>/dev/null | head -5


