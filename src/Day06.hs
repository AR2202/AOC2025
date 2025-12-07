module Day06 (day06a, day06b) where

import Data.List (transpose)
import Text.Regex

day06a :: IO ()
day06a = do
  input <- readFile "input/Day06.txt"
  let ls = lines input
  let symbols = map words ls
  let problems = transpose symbols
  let nums = map (map read . init) problems
  let functions = map last problems
  let answers = zipWith applyFunction functions nums
  print $ sum answers

applyFunction :: (Foldable t, Num a) => [Char] -> t a -> a
applyFunction "*" = product
applyFunction "+" = sum

-- Part 2
day06b :: IO ()
day06b = do
  input <- readFile "input/Day06.txt"
  let ls = lines input
  let parts = transpose ls
  let problems = separateOn (mkRegex "^[ ]+$") parts
  let functions = map (return . last . head) problems
  let numbers =
        map
          ( map read
              . concatMap words
              . (\x -> (init . head) x : tail x)
          )
          problems
  let answers = zipWith applyFunction functions numbers
  print $ sum answers

separateOn :: Regex -> [String] -> [[String]]
separateOn _ [] = []
separateOn regex xs =
  takeWhile (noRegexMatch regex) xs
    : separateOn
      regex
      (tailOrEmpty (dropWhile (noRegexMatch regex) xs))

noRegexMatch :: Regex -> String -> Bool
noRegexMatch regex str = matchRegex regex str == Nothing

tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty xs = tail xs
