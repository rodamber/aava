#!/usr/bin/env bash

# exec 2> /dev/null
executable="./om"

echo "n,m,time(ms),heap(bytes),comparisons"

for i in {1..5}; do
    for j in {1..10}; do
        if (( i >= j )); then
            input="data/txt-$i-pat-$j.in"

            # Measure time.
            start=$(date +%s%N);
            cmps="$($executable < $input)";
            end=$(date +%s%N);

            # Measure heap usage.
            valgrind --log-file="val.out" $executable < $input > /dev/null
            heap="$(head -n 10 val.out | tail -1 | cut -d " " -f 11 | sed "s/,//g")"
            rm val.out

            # Print
            diff_nano=$( echo "$end - $start" | bc -l );
            diff_mili=$( echo "scale=2; $diff_nano / 1000000" | bc -l );

            printf "$i,$j,$diff_mili,$heap,$cmps\n"
        fi;
    done;
done;
