#!/usr/bin/env bash

PROJECT_ROOT=./..

exec &> data/memory_results.dat

for file in ../graphs/in/*
do
    exec < $file

    read V E

    echo -n `expr $V + $E`
    echo -n ";"

    valgrind --log-file="val.out" ../../src/a.out < $file > /dev/null
    head -n 10 val.out | tail -1 | cut -d " " -f 11 | sed "s/,//g"
done

exec &> data/time_results.dat

for file in ../graphs/in/*
do
    exec < $file

    read V E

    echo -n `expr $V \* $E`
    echo -n " "

    TIMEFORMAT=%R
    time ../../src/a.out < $file > /dev/null
done
