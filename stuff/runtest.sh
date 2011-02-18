#!/bin/bash
#
# usage: runtest.sh COMPILER FILENAME OPTION ...

compiler="$1"
file="$2"

shift 2

options="$@"

if test -e "${file}.options"; then
    options="$options "`cat ${file}.options`
fi

echo "======================================== $file"
exec $compiler $options $file
