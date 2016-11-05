#!/usr/bin/env sh

if test 0 -eq $#; then
    echo "Invalid number of arguments"
    echo "Usage: $0 path/to/netlist"
    exit 1
fi

path=$1
name=$(basename $path .net)
./compile-net $path > "$name.s" && gcc "$name.s" helper.c -o "$name.out"

