#!/bin/sh

target="tmp/compiled"
mkdir -p tmp
hlint $1 && \
ghc $1 euler.hs --make -o $target && \
rm -f *.hi *.o && ./$target && \
rm -f $target
