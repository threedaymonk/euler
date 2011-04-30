{-
  2520 is the smallest number that can be divided by each of the numbers from 1
  to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the
  numbers from 1 to 20?
-}

module Main where
import Data.List
import Data.Maybe

main = putStrLn $ show ans

ans = foldl (*) 1 (map product (minimalFactors 20))
  where product (a, b) = a ^ b

minimalFactors n = map maxOccurrences (filter isPrime numbers)
  where
    maxOccurrences i = (i, maximum $ map length $ map (filter (== i)) pfList)
    pfList = map primeFactors numbers
    numbers = [2 .. n]

-- Copied from solution to problem 3:

primeFactors n = fst $ primeFactors' ([], n)

primeFactors' (xs, 1) = (xs, 1)
primeFactors' (xs, n) = primeFactors' (f : xs, n `div` f)
  where f = smallestPrimeFactor n

smallestPrimeFactor n = fromJust $ find isPrimeFactor [2..]
  where isPrimeFactor x = isFactor n x && isPrime x


isPrime n = null $ filter (isFactor n) $ takeWhile notTooBig [2..]
  where notTooBig m = m * m <= n

isFactor n m = n `rem` m == 0
