#!/bin/bash
IFS='`'
read line
len=${#line}
[ "$1" != '' ] && len=$1
mid=$((($(tput cols)-$len)/2))
pad=%"$mid"s
printf "$pad"
echo "$line"
while read line; do
	printf "$pad"
	echo "$line"
done
