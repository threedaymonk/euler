{-
  Starting in the top left corner of a 2x2 grid, there are 6 routes (without
  backtracking) to the bottom right corner.
     _ _   _     _
        |   |_    |   |_ _  |_    |
        |     |   |_      |   |_  |_ _

  How many routes are there through a 20x20 grid?
-}

module Main where

main = putStrLn $ show $ routes 20

routes n = combinations (n*2) n

factorial = product . enumFromTo 1

combinations n r = (factorial n) `div` ((factorial (n-r)) * (factorial r))
