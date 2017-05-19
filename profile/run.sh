#!/usr/bin/env bash

executable="./om"

echo "n m time heap comparisons"

for input in data/*; do
    # Measure time.
    start=$(date +%s%N);
    cmps="$($executable < $input &)";
    end=$(date +%s%N);

    # Measure heap usage.
    valgrind --log-file="val.out" $executable < $input > /dev/null &

    wait

    heap="$(head -n 10 val.out | tail -1 | cut -d " " -f 11 | sed "s/,//g")"
    rm val.out

    # Print
    diff_nano=$(echo "$end - $start" | bc -l);
    diff_mili=$(echo "scale=2; $diff_nano / 1000000" | bc -l);

    printf "$input $diff_mili $heap $cmps\n"
done;
