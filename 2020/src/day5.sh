#!/bin/bash

decode() {
    python3 -c "$(echo -e 'import sys;\nfor line in sys.stdin: print(int(line, 2))')"
}

nums="$(gsed -e '{ s/F/0/g; s/B/1/g; s/L/0/g; s/R/1/g }' input/day5.txt | decode | sort -n)"
echo "$nums"

paste <(echo "$nums") <(echo "$nums" | tail -n+2) <(echo "$nums" | tail -n+3) |
    while read -r line ; do
        one="$(echo "$line" | cut -f1)"
        two="$(echo "$line" | cut -f2)"
        three="$(echo "$line" | cut -f3)"
        if [ "$three" -ne $(( one + 2 )) ] ; then
            echo "$line"
        fi
    done
