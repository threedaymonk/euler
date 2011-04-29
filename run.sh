#!/bin/sh

mkdir -p tmp
ghc $1 -o tmp/compiled && rm *.hi *.o && ./tmp/compiled
