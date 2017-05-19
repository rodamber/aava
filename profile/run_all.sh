#!/usr/bin/env bash

gcc-4.7 -O3 ../cbits/online-matching/online_matching.c -o om
gcc-4.7 -O3 ../cbits/link-cut/link_cut.c -o lc

# echo "Starting naive..."
# sed -i 's/^\(K\|B\)/N/' tests/*

# echo "Running naive..."
# ./run.sh > naive-bad.dat

# echo "Starting KMP..."
# sed -i 's/^\(N\|B\)/K/' tests/*

# echo "Running KMP..."
# ./run.sh > kmp.dat

# echo "Starting BM..."
# sed -i 's/^\(N\|K\)/B/' tests/*

# echo "Running BM..."
# ./run.sh > bm-sublinear.dat

echo "Running Link-Cut..."
./run.sh ./lc > data/link-cut.dat

echo "Finished!"

