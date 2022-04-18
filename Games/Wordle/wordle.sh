#!/bin/bash

cd ~/.config/wordle
# create individual workspace

stty -echo
trap "tput sgr0; stty echo" EXIT

cat </dev/null >filter

function runFilter {
    filterlen=$(wc -c <filter)
    if ((filterlen == 0)); then
        cat words.txt >filtered
    else
        bash <(tr '\n' '|' <filter) <words.txt >filtered
    fi
    numFiltered=$(wc -l <filtered)
}

function addFilter {
    last=$((linelen - 1))
    for i in $(seq 0 $last | tr '\n' ' '); do
        letter=${word:i:1}
        result=${results:i:1}
        if [ "$result" == 'j' ]; then
            filterlen=$(wc -c <filter)
            ((filterlen == '0')) || echo >>filter
            printf 'grep ' >>filter
            ((i!=0)) && printf '.%.0s' $(seq $i | tr '\n' ' ') >>filter
            printf "$letter" >>filter
            ((i!=last)) && printf '.%.0s' $(seq $((last - i)) | tr '\n' ' ') >>filter
        elif [ "$result" == 'k' ]; then
            filterlen=$(wc -c <filter)
            ((filterlen == '0')) || echo >>filter
            printf 'grep ' >>filter
            printf "${letter}\n" >>filter
            printf 'grep -v ' >>filter
            ((i!=0)) && printf '.%.0s' $(seq $i | tr '\n' ' ') >>filter
            printf "$letter" >>filter
            ((i!=last)) && printf '.%.0s' $(seq $((last - i)) | tr '\n' ' ') >>filter
        else
            # assumes first duplicate will be marked included
            filterlen=$(wc -c <filter)
            ((filterlen == '0')) || echo >>filter
            printf 'grep -v ' >>filter
            printf "$letter" >>filter
        fi
    done
}

function printCurrentSuggestion {
    if ((y==0)); then
        ((x!=0)) && tput cub $x
        tput el
        ((x!=0)) && tput cuf $x
    else
        tput cub $x
        printf "$(head -n$y <filtered | tail -n1)"
        ((linelen != x)) && tput cub $((linelen - x))
    fi
}

x=0
y=0
linelen=5
cat </dev/null >word
cat </dev/null >results

runFilter

# suggestion is sticky after new letter: 0 -> 0 | x -> 1
# clear suggestion with ^L
# only support live front filter, but arbitrary back filters can be applied with new word-row
# highligh letters according to "why" they're there in suggestion matched/present/prefix/suggested
# switch between legal guesses, and potential solutions
# be able to play today's wordle

write=true
while read -n1 key; do
    key=$(cat -v <<<"$key")
	echo "$key" >key
    if $write; then
		# don't process start of arrow keys
		grep '\[' <<<"$key" >/dev/null && continue
        if [ "$key" == '^?' ] || [ "$key" == '^H' ]; then
            if (($x != 0)); then
                ((x--))
                printf '\b \b'
            fi
            printf '^H' >>word
        elif (($x == $linelen)); then
            x=0
            tput cub $linelen
            write=false
            word=$(sed 's/.^.//g' <word)
        elif [ "$key" == '^N' ] || [ "$key" == 'B' ]; then
            if ((numFiltered != 0)); then
                ((y++))
                y=$((y % ((numFiltered+1))))
            fi
            printCurrentSuggestion
        elif [ "$key" == '^P' ] || [ "$key" == 'A' ]; then
            if ((numFiltered != 0)); then
                ((y--))
                ((y < 0)) && y=$numFiltered
                y=$((y % ((numFiltered+1))))
            fi
            printCurrentSuggestion
        else
            printf "$key"; ((x++))
            printf "$key" >>word
        fi
    else
        if [ "$key" == '^?' ] || [ "$key" == '^H' ]; then
            tput sgr0
            if (($x != 0)); then
                ((x--))
                printf "\b${word:x:1}\b"
                printf '^H' >>results
            fi
        elif (($x == $linelen)); then
            tput sgr0
            echo
            x=0
            tput cub $linelen
            write=true
            results=$(sed 's/.^.//g' <results)
            addFilter
            runFilter
            cat </dev/null >word
            cat </dev/null >results
        elif [ "$key" == 'k' ]; then
            tput setab 3
            printf "${word:x:1}"
            ((x++))
            printf 'k' >>results
        elif [ "$key" == 'j' ]; then
            tput setab 2
            printf "${word:x:1}"
            ((x++))
            printf 'j' >>results
        else
            tput cuf1
            ((x++))
            printf 'f' >>results
        fi
    fi
done

