{-
  A palindromic number reads the same both ways. The largest palindrome made
  from the product of two 2-digit numbers is 9009 = 91 x 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
-}

module Main where
import Data.List

main = putStrLn $ show ans

ans = maximum [ i*j | i <- factors, j <- factors, isPalindrome(i*j) ]
  where factors = [100..999]

isPalindrome n = (show n) == (reverse (show n))
