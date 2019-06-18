#!/usr/bin/env bash

folders=("small" "small2" "small3" "small4" "medium")

echo "Delayed"
cp ../experimental/settings/del.hs ComplConfig.hs
make release
for item in ${folders[*]}
do
    echo ${item}
    python3 ../experimental/experimental.py ./Main ../experiments/${item}/ >> ../results/
done

echo "Direct"
cp ../experimental/settings/dir.hs ComplConfig.hs
make release
for item in ${folders[*]}
do
    echo ${item}
    python3 ../experimental/experimental.py ./Main ../experiments/${item}/ >> ../results/
done

echo "Combination"
cp ../experimental/settings/comb.hs ComplConfig.hs
make release
for item in ${folders[*]}
do
    echo ${item}
    python3 ../experimental/experimental.py ./Main ../experiments/${item}/ >> ../results/
done
