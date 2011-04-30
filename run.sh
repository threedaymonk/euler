#!/bin/sh

mkdir -p tmp
ghc $1 euler.hs --make -o tmp/compiled && rm *.hi *.o && ./tmp/compiled
