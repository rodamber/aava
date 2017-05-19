#!/usr/bin/env bash

gcc-4.7 -O3 ../cbits/online-matching/online_matching.c -o om

echo "Starting naive..."
sed -i 's/^\(K\|B\)/N/' data/*

echo "Running naive..."
./run.sh > naive.dat

echo "Starting KMP..."
sed -i 's/^\(N\|B\)/K/' data/*

echo "Running KMP..."
./run.sh > kmp.dat

echo "Starting BM..."
sed -i 's/^\(N\|K\)/B/' data/*

echo "Running BM..."
./run.sh > bm.dat

echo "Finished!"

