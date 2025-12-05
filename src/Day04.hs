module Day04 (day04a, day04b) where

import Data.List (transpose)
import qualified Data.Map as M
import qualified Data.Set as S

day04a :: IO ()
day04a = do
  rollPositions <- prepareInput04
  let numNeighbours = S.map (countNeighbours rollPositions . makeNeighbours) rollPositions
  let accessible = (S.size . S.filter (< 4)) numNeighbours

  print accessible

day04b :: IO ()
day04b = do
  rollPositions <- prepareInput04
  let inititalRollNumber = S.size rollPositions
  let removed = removeAll rollPositions
  print $ inititalRollNumber - length removed

prepareInput04 :: IO (S.Set (Int, Int))
prepareInput04 = do
  input <- readFile "input/Day04.txt"
  let ls = lines input
  let withCoords = addCoordinates ls
  let rollPositions = map fst $ onlyRollCoords withCoords
  return $ S.fromList rollPositions

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [String] -> [((Int, Int), Char)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

countNeighbours :: S.Set (Int, Int) -> [(Int, Int)] -> Int
countNeighbours grid neighbours =
  length $ filter (`S.member` grid) neighbours

makeNeighbours (x, y) = 
  [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1], (a, b) /= (x, y)]

onlyRollCoords :: [(a, Char)] -> [(a, Char)]
onlyRollCoords  = filter (\x -> snd x == '@') 

removeAll :: S.Set (Int, Int) -> S.Set (Int, Int)
removeAll rollPos =
  go
    rollPos
    ( M.fromList $
        zip
          (S.toList rollPos)
          (map makeNeighbours $ S.toList rollPos)
    )
  where
    go rollPositions neighboursdict
      | length rollPositions == length moreThan4Neighbours = rollPositions
      | otherwise = go moreThan4Neighbours neighboursdict
      where
        moreThan4Neighbours =
          S.filter
            ( (>= 4)
                . countNeighbours rollPositions
                . flip (M.findWithDefault []) neighboursdict
            )
            rollPositions
