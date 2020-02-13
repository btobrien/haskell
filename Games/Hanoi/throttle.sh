#!/bin/bash

delay=${1:-0.12}
while read -n1 x; do
	sleep $delay
	printf "$x"
done
echo

