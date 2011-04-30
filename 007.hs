{-
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
  that the 6th prime is 13.

  What is the 10001st prime number?
-}

module Main where

main = putStrLn $ show $ nthPrime 10001

nthPrime 1 = 2
nthPrime n = head $ filter isPrime [((nthPrime (n-1)) + 1)..]

isPrime n = null $ filter (isFactor n) $ takeWhile notTooBig [2..]
  where
    notTooBig m = m * m <= n
    isFactor n m = n `rem` m == 0
