module Day04 (day04a, day04b) where

import Data.List (transpose)

day04a :: IO ()
day04a = do
  input <- readFile "input/Day04.txt"
  let ls = lines input
  let withCoords = addCoordinates ls
  let rollPositions = map fst $ onlyRollCoords withCoords
  let numNeighbours = map (countNeighbours rollPositions) rollPositions
  let accessible = (length . filter (< 4)) numNeighbours

  print accessible

day04b :: IO ()
day04b = do
  input <- readFile "input/Day04.txt"
  let ls = lines input
  let withCoords = addCoordinates ls
  let rollPositions = map fst $ onlyRollCoords withCoords
  let inititalRollNumber = length rollPositions
  let removed = removeAll rollPositions
  print $ inititalRollNumber - length removed

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [String] -> [((Int, Int), Char)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

countNeighbours grid (x, y) =
  length $ filter (`elem` grid) [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y)]

onlyRollCoords ls = filter (\x -> snd x == '@') ls

removeOnce rollPositions = filter ((>= 4) . countNeighbours rollPositions) rollPositions

removeAll rollPositions
  | all ((>= 4) . countNeighbours rollPositions) rollPositions = rollPositions
  | otherwise = removeAll $ filter ((>= 4) . countNeighbours rollPositions) rollPositions
