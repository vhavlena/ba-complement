#!/usr/bin/env bash

folders=("small" "small2" "small3" "small4" "medium")

echo "Algorithm comparison"
for item in ${folders[*]}
do
    echo ${item}
    python3 ../experimental/experimental-compare.py ../src/Main ... ../experiments/${item}/ >> ../results/
done
