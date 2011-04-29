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

isPalindrome n = n == reverseDigits n

reverseDigits n = sum $ map reversedValueAt [1 .. (numberOfDigits)]
  where
    digitAt i = (n `mod` (10 ^ i)) `div` (10 ^ (i - 1))
    reversedValueAt i = digitAt i * (10 ^ (numberOfDigits - i))
    numberOfDigits = ceiling $ logBase 10 (fromIntegral n)
