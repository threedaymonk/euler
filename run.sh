#!/bin/sh
set -e

wd=tmp
target="$wd/$(basename $1 .hs)"

mkdir -p $wd
hlint $1
ghc $1 euler.hs --make -o $target
rm -f *.hi *.o
./$target
