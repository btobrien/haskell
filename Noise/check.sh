#!/bin/bash

source ~/.config/noise/import

word_length=$1
errors=$2

if [ "$3" == 'cat' ]; then
    encoder=cat
    decoder=cat
else
    encoder="$3 encode"
    decoder="$3 decode"
fi

Z "(2,$word_length)" >words 

# output codeword length
echo $($encoder <words | head -n1 | tr -d '\n' | wc -m)

$encoder <words | flipbit $errors | $decoder >decoded
diff words <(uniq decoded) >/dev/null
