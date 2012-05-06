{-
  Using names.txt, a 46K text file containing over five-thousand first names,
  begin by sorting it into alphabetical order. Then working out the
  alphabetical value for each name, multiply this value by its alphabetical
  position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, which is
  worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
  would obtain a score of 938 x 53 = 49714.

  What is the total of all the name scores in the file?
-}

module Main where
import Data.List
import Text.ParserCombinators.Parsec

nameList = name `sepBy` char ','
  where
    name = do
      char '"'
      content <- many $ noneOf "\""
      char '"'
      return content

main = do
  result <- parseFromFile nameList "names.txt"
  case result of
    Left err    -> print err
    Right names -> print $ sum $ scores $ sort names

scores names = map score $ names `zip` [1..]
  where score (name, rank) = rank * alphaValue name

alphaValue name = sum $ map pos name
  where pos a = fromEnum a - fromEnum 'A' + 1
