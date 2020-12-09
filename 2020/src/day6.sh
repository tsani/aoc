#!/bin/bash

count() {
    while read -r line ; do
        echo "$line" | gsed 's/\(.\)/\1\n/g' | sort | uniq | wc -l
    done
}

plusify() {
    gsed '{s/\s\+//; /./ {H;$!d}; x; s/\n//; s/\n/+/g}'
}

input="$(gsed '{ /./ {H;$!d}; x; s/\n//g; }' input/day6.txt)"

answer="$(echo "$input" | count | plusify | bc)"
offbyone="$(echo "$input" | wc -l)"
echo $(( answer - offbyone ))
