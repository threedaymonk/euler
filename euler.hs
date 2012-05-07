module Euler where
import Data.List
import Data.Maybe

primeFactors n = fst $ primeFactors' ([], n)

primeFactors' (xs, 1) = (xs, 1)
primeFactors' (xs, n) = primeFactors' (f : xs, n `div` f)
  where f = smallestPrimeFactor n

smallestPrimeFactor n = fromJust $ find isPrimeFactor [2..]
  where isPrimeFactor x = isFactor n x && isPrime x

isPrime n = not $ any (isFactor n) $ takeWhile notTooBig [2..]
  where notTooBig m = m * m <= n

isFactor n m = n `rem` m == 0

-- A strict implementation of maximumBy, from
-- http://hackage.haskell.org/trac/ghc/ticket/3416
maximumBy'        :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ []   =  error "List.maximumBy: empty list"
maximumBy' cmp xs =  foldl1' maxBy xs
  where
    maxBy x y = case cmp x y of
                GT -> x
                _  -> y

sumDigits n = sumDigits' 0 n
  where
    sumDigits' acc    0 = acc
    sumDigits' acc rest = sumDigits' (acc + (rest `rem` 10)) (rest `div` 10)

properDivisors n = filter ((==0) . rem n) [1..n `div` 2]
