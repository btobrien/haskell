#!/bin/bash

set -o pipefail

config=$HOME/.config/hanoi

size=${1:-5}
mkdir -p $config/scores/

player=$2

isbot=false
if ! [[ -t 0 ]]; then
    isbot=true
fi

if [ "$player" == '' ]; then
    echo 'error: empty player name' >&2
    exit 1
fi

/usr/bin/time -f'%e' playnoi $size 2>$config/last

[ "$?" == 0 ] || exit 1

echo
cat $config/last

echo

if $isbot; then
    echo "nice job!"
    echo "too bad you're a bot"
    echo
else
    echo "$(cat $config/last) $player" >>$config/scores/$size
fi

echo 'leader board'
echo '------------'
sort -n $config/scores/$size 2>/dev/null | head -5


