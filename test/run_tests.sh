#!/bin/sh

echo "Testing all tests"

for i in $1/*.in;
do
    echo "=== Testing $i";
    cat $i | ../src/$1 > $1/myout.d/`basename $i .in`.myout;
    diff $1/myout.d/`basename $i .in`.myout $1/`basename $i .in`.out;
done

echo "Done."
