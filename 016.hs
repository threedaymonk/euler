{-
  What is the sum of the digits of the number 2^1000?
-}

module Main where

main = putStrLn $ show $ ans $ 2^1000

ans n = f 0 n
  where
    f acc 0 = acc
    f acc n = f (acc + (n `rem` 10)) (n `div` 10)
