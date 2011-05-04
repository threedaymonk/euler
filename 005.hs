{-
  2520 is the smallest number that can be divided by each of the numbers from 1
  to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the
  numbers from 1 to 20?
-}

module Main where
import Euler

main = print ans

ans = product $ map pow $ minimalFactors 20
  where pow (a, b) = a ^ b

minimalFactors n = map maxOccurrences (filter isPrime numbers)
  where
    maxOccurrences i = (i, maximum $ map (length . filter (== i)) pfList)
    pfList = map primeFactors numbers
    numbers = [2 .. n]
