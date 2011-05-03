{-
  You are given the following information, but you may prefer to do some
  research for yourself.

  * 1 Jan 1900 was a Monday.

  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.

  * A leap year occurs on any year evenly divisible by 4, but not on a century
    unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth century
  (1 Jan 1901 to 31 Dec 2000)?
-}

module Main where

main = putStrLn $ show $ ans

data Day = Day Int Int Int Int -- year month day weekday
                               -- weekday is 0-indexed: 0 == Sun

ans = length $
      filter isSunday1st $
      takeWhile (yearLt 2001) $
      dropWhile (yearLt 1901) $
      daysFrom [Day 1900 1 1 1]
  where
    yearLt a (Day y _ _ _) = y < a
    isSunday1st (Day _ _ 1 0) = True
    isSunday1st (Day _ _ _ _) = False

daysFrom :: [Day] -> [Day]
daysFrom days = days ++ (daysFrom [tomorrow $ head days])

tomorrow :: Day -> Day
tomorrow (Day y m d w)
  | d < (daysInMonth m y) = Day y m (d+1) w'
  | m < 12                = Day y (m+1) 1 w'
  | otherwise             = Day (y+1) 1 1 w'
  where w' = (w + 1) `mod` 7

daysInMonth m y
  | m `elem` [4, 6, 9, 11] = 30
  | m == 2 && isLeap y = 29
  | m == 2 = 28
  | otherwise = 31
  where isLeap y = (y `rem` 4) == 0 &&
                   ((y `rem` 100) /= 0 || (y `rem` 400) == 0)
