#!/bin/bash

config=$HOME/.config/payout
export PATH=$config/bin:$PATH

set -o pipefail

mkdir -p "$config/records"
cd $config

if ! [ "$1" == '' ]; then
	name=$2
	if ! grep "$name " <address_book >/dev/null; then
		echo "ERROR: name=$name not recognized"
		exit 1
	elif   [ "$1" == 'list' ]; then
		cat $config/records/* | grep "$name" | cut -d' ' -f2
	elif [ "$1" == 'total' ]; then
		cat $config/records/* | grep "$name" | cut -d' ' -f2 | paste -sd+ | bc
	elif [ "$1" == 'series' ]; then
		cat $config/records/* | grep "$name" | cut -d' ' -f2 | awk '{total += $0; $0 = total}1'
	elif [ "$1" == 'plot' ]; then
		cat $config/records/* | grep "$name" | cut -d' ' -f2 | { echo 0; awk '{total += $0; $0 = total}1'; } | plot lines
	elif [ "$1" == 'bigplot' ]; then
		cat $config/records/* | grep "$name" | cut -d' ' -f2 | { echo 0; awk '{total += $0; $0 = total}1'; } | plot big lines $name
	fi
	exit
fi

today=$(date +"%Y%m%d")
sender='tp2@wolve.com'
subject='Results'

touch address_book
echo >results
echo

function edit_results
{
    vim results
    sort results
    echo
}

function set_addresses
{
    addresses=''
    unrecognized=true
    while $unrecognized; do
        unrecognized=false
        names=$(cut -d' ' -f1 <results | tr '\n' ' ')
        for name in $names; do
            if ! grep "$name " <address_book >/dev/null; then
                unrecognized=true
                echo "ERROR: name=$name not recognized"
                echo "would you like to add $name to the address book? (y/n)"
                read reply
                echo
                if [ "$reply" == 'y' ]; then
                    echo "$name" >> address_book
                    vim address_book
                else
                    edit_results
                fi
                break
            fi
        done
    done

    for name in $names; do
        addresses=$addresses,$(grep "$name " <address_book | cut -d' ' -f2)
    done
}

function set_payouts
{
    edit_results
    set_addresses
    while ! pay <results | tee output.txt | sort; do
        echo
        echo 'would you like to edit? (y/n)'
        read reply
        echo
        if [ "$reply" == 'y' ]; then
            edit_results
            set_addresses
        else
			exit 1
        fi
    done
    echo
}

function set_reply
{
    echo 'would you like to send results? (y/n)'
    read reply
    echo
}

while set_payouts && set_reply && ! [ "$reply" == 'y' ]; do
    continue
done

if mailx -S smtp=mdsmpt:25 -r "$sender" -s "$subject" "$addresses" <output.txt; then
    echo "sent"
    cat results >> records/$today.txt
else
    echo "failed to send"
    exit 1
fi

