{-
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we
  get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
-}

module Main where

main = putStrLn (show ans)

multiples n = [ i | i <- [1..(n - 1)], i `rem` 3 == 0 || i `rem` 5 == 0 ]

ans = foldl (+) 0 (multiples 1000)
