module Lib
  ( someFunc,
    makeCoordinates,
    addCoordinates,
    addCoordinatesTransposed,
  )
where

import Data.List (transpose)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [String] -> [((Int, Int), Char)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

addCoordinatesTransposed :: [String] -> [((Int, Int), Char)]
addCoordinatesTransposed ls = zip coordlist chars
  where
    coordlist = makeCoordinates (length ls) ((length . head) ls) 
    chars = concat ls
