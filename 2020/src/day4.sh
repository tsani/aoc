#!/bin/bash

VALID1='select(.byr and .iyr and .eyr and .hgt and .hcl and .ecl and .pid)'
VALID2='
select(
.byr and 1920 <= (.byr|tonumber) and (.byr|tonumber) <= 2002
and .iyr and 2010 <= (.iyr|tonumber) and (.iyr|tonumber) <= 2020
and .eyr and 2020 <= (.eyr|tonumber) and (.eyr|tonumber) <= 2030
and .hgt and (
   (.hgt|test("cm")) and 150 <= (.hgt|sub("cm";"")|tonumber) and (.hgt|sub("cm";"")|tonumber) <= 193
 or
   (.hgt|test("in")) and 59 <= (.hgt|sub("in";"")|tonumber) and (.hgt|sub("in";"")|tonumber) <= 76
)
and .hcl and (.hcl|test("^#[0-9a-f]{6}$"))
and .ecl and (.ecl == "amb" or .ecl == "blu" or .ecl == "brn" or .ecl == "gry" or .ecl == "grn" or .ecl == "hzl" or .ecl == "oth")
and .pid and (.pid|test("^[0-9]{9}$"))
)
'

check_all() {
    gsed \
        -e 's/\(\w\+\):\(\S\+\)/"\1":"\2"/g' \
        -e '/./{H;$!d} ; x ; s/\n/, /g' \
        -e 's/" /", /g' \
        -e 's/^, //' \
        -e 's/.*/{\0}/' \
        input/day4.txt |
        tee day4.json |
        jq -c "$1" |
        wc -l
}

check_all "$VALID1"
check_all "$VALID2"
