{-
  2520 is the smallest number that can be divided by each of the numbers from 1
  to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the
  numbers from 1 to 20?
-}

module Main where
import Euler

main = putStrLn $ show ans

ans = foldl (*) 1 (map product (minimalFactors 20))
  where product (a, b) = a ^ b

minimalFactors n = map maxOccurrences (filter isPrime numbers)
  where
    maxOccurrences i = (i, maximum $ map length $ map (filter (== i)) pfList)
    pfList = map primeFactors numbers
    numbers = [2 .. n]
