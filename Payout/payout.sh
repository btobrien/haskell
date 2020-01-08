#!/bin/bash

config=$HOME/.config/payout
export PATH=$config/bin:$PATH

set -o pipefail

mkdir -p "$config/records"
cd $config

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
                echo "ERROR: name not recognized"
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
        distribute_missing <results | tee output.txt | sort
        echo
        echo 'would you like to distribute accordingly? (y/n)'
        read reply
        echo
        if [ "$reply" == 'y' ]; then
            cat <output.txt >results
        else
            edit_results
            set_addresses
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





