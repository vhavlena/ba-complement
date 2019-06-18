#!/usr/bin/env bash

folders=("small" "small2" "small3" "small4" "medium")

for item in ${folders[*]}
do
    python3 ../experimental/experimental.py ./Main ../experiments/${item}/ >> ../results/
done
