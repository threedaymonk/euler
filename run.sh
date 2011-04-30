#!/bin/sh

mkdir -p tmp
ghc $1 --make -o tmp/compiled && rm *.hi *.o && ./tmp/compiled
