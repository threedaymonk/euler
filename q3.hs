{-
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
-}

module Main where
import Data.List
import Data.Maybe

main = putStrLn $ show ans

ans = head $ primeFactors 600851475143

primeFactors n = fst $ primeFactors' ([], n)

primeFactors' (xs, 1) = (xs, 1)
primeFactors' (xs, n) = primeFactors' (f : xs, n `div` f)
  where f = smallestPrimeFactor n

smallestPrimeFactor n = fromJust $ find isPrimeFactor [2..]
  where isPrimeFactor x = isFactor n x && isPrime x

isPrime n = null $ filter (isFactor n) $ takeWhile notTooBig [2..]
  where notTooBig m = m * m <= n

isFactor n m = n `rem` m == 0
