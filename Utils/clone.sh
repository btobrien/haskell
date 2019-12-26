#!/bin/bash

app=$1
user_host=$2

if [[ "$user_host" == '' ]]; then
    echo "error: missing <user>@<host>" >&2
    exit 1
fi

if grep @ <<< $user_host; then
    user=$(cut -d@ -f1 <<< $user_host)
else 
    user=$(whoami)
fi

scp $0 $user_host:/home/$user/bin/$(basename $0)
scp ~/bin/$app $user_host:/home/$user/bin/$(basename $app)

find -type d -name ~/.config/$app 2>/dev/null || exit

scp -pr ~/.config/$app $user_host:/home/$user/.config/$(basename $app)
