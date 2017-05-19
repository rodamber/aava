#!/usr/bin/env bash

executable="$1"

if [ "$executable" == "./om" ]; then
    echo "n m time heap comparisons"
elif [ "$executable" == "./lc" ]; then
    echo "n time heap"
else
    echo "Bad argument: $1"
    exit -1
fi

for input in tests/*; do
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

    if [ "$executable" == "./om" ]; then
        printf "$input $diff_mili $heap $cmps\n"
    elif [ "$executable" == "./lc" ]; then
        printf "$input $diff_mili $heap\n"
    fi

done;
