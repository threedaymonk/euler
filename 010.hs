{-
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
-}

module Main where
import qualified Data.Map as Map
import Data.List

main = putStrLn $ show $ ans 2000000

-- Use a strict left fold to avoid running out of stack space
ans n = foldl' (+) 0 $ takeWhile (< n) primes

primes = sieve [2..]

-- The simple implementation from O'Neill 2008

sieve xs = sieve' xs Map.empty
  where
    sieve' []     table = []
    sieve' (x:xs) table =
      case Map.lookup x table of
        Nothing    -> x : sieve' xs (Map.insert (x*x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
      where
        reinsert table prime = Map.insertWith (++) (x+prime) [prime] table
