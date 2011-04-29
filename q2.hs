module Main where

main = putStrLn (show ans)

ans = sum $ filter even $ takeWhile (<4000000) $ map fib [1..]

fib n
    | n == 0 = 0
    | n == 1 = 1
    | n  > 1 = fib (n-1) + fib (n-2)
