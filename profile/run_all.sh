#!/usr/bin/env bash

gcc-4.7 -O3 ../cbits/online-matching/online_matching.c -o om

echo "Starting naive..."
# sed -i 's/^\(K\|B\)/N/' data/txt-*-pat-*.in

echo "Running naive..."
./run.sh > naive-test.csv

echo "Starting KMP..."
# sed -i 's/^\(N\|B\)/K/' data/txt-*-pat-*.in

echo "Running KMP..."
./run.sh > kmp-test.csv

# echo "Starting BM..."
# sed -i 's/^\(N\|K\)/B/' data/txt-*-pat-*.in

# echo "Running BM..."
# ./run.sh > bm-test.csv

echo "Finished!"

