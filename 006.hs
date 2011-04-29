{-
  The sum of the squares of the first ten natural numbers is,

      1^2 + 2^2 + ... + 10^2 = 385
  
  The square of the sum of the first ten natural numbers is,

      (1 + 2 + ... + 10)^2 = 55^2 = 3025
  
  Hence the difference between the sum of the squares of the first ten natural
  numbers and the square of the sum is 3025  385 = 2640.

  Find the difference between the sum of the squares of the first one hundred
  natural numbers and the square of the sum.
-}

module Main where

main = putStrLn $ show $ ans 100

ans n = (sqSum n) - (sumSq n)
  where
    sumSq n = sum $ map (^2) [1 .. n]
    sqSum n = (sum [1 .. n]) ^ 2
