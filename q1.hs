module Main where

main = putStrLn (show ans)

multiples n = [ i | i <- [1..(n - 1)], i `rem` 3 == 0 || i `rem` 5 == 0 ]

ans = foldl (+) 0 (multiples 1000)
