#!/bin/bash

config=$HOME/.config/hanoi
mkdir -p $config/scores/

size=${1:-5}
player=$2

isbot=false
if ! [[ -t 0 ]]; then
    isbot=true
fi

if [ "$player" == '' ]; then
    echo 'error: empty player name' >&2
    exit 1
fi

/usr/bin/time toi $size 2>$config/last 

[ "$?" == 0 ] || exit 4

echo
score=$(awk '{ print $1 }' <$config/last)
echo $score
echo

if $isbot; then
    echo "nice job!"
    echo "too bad you're a bot"
    echo
else
    echo "$score $player" >>$config/scores/$size
fi

echo 'leader board'
echo '------------'
sort -n $config/scores/$size 2>/dev/null | head -5


