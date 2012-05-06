{-
  By starting at the top of the triangle below and moving to adjacent numbers
  on the row below, the maximum total from top to bottom is 23.

         3
        7 4
       2 4 6
      8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle below:

      (see 018.txt)

  NOTE: As there are only 16384 routes, it is possible to solve this problem by
  trying every route. However, Problem 67, is the same challenge with a
  triangle containing one-hundred rows; it cannot be solved by brute force, and
  requires a clever method! ;o)
-}

module Main where

main = do
  text <- readFile "018.txt"
  print $ maxPath $ readTriangle text

readTriangle :: String -> [[Int]]
readTriangle s = readTriangle' 1 (map read (words s))
readTriangle' _ [] = []
readTriangle' n w = take n w : readTriangle' (n + 1) (drop n w)

maxValueAt 0 0 t = head $ head t
maxValueAt r c t
  | c < 0 = 0
  | c > r = 0
  | otherwise =
      t !! r !! c +
      max (maxValueAt (r - 1) (c - 1) t)
          (maxValueAt (r - 1)  c      t)

maxPath t = maximum $ map maxValues [0..rmax]
  where
    maxValues c = maxValueAt rmax c t
    rmax = length t - 1
