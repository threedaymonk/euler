{-
  What is the sum of the digits of the number 2^1000?
-}

module Main where
import Euler

main = putStrLn $ show $ sumDigits $ 2^1000
