module Day07 (day07a) where


import Data.List (nub, sort)
import qualified Data.Map as M
import Lib ( addCoordinatesTransposed)

day07a :: IO ()
day07a = do
  input <- readFile "input/Day07.txt"
  let withCoordsT = addCoordinatesTransposed $ lines input
  let splittersT = filter ((== '^') . snd) withCoordsT
  let splitterlist = sort $ map fst splittersT
  let start = fst $ head $ filter ((== 'S') . snd) withCoordsT
  let splitcount = beamTravel splitterlist [snd start] 0
  let timecount = beamTravel' splitterlist [snd start] (M.singleton (snd start) 1)
  putStrLn "part 1"
  print splitcount
  putStrLn "part 2"
  print timecount

freeBeamTravel ::
  (Eq a2, Eq a1, Num a1) =>
  [(a2, a1)] ->
  [a1] ->
  (([a1], [(a2, a1)]), Int)
freeBeamTravel ls ys = ((nub $ ysRemaining ++ newBeams, rest), length beamsSplit)
  where
    found = dropWhile ((`notElem` ys) . snd) ls
    xline = fst $ head found
    rest = dropWhile ((== xline) . fst) found
    line = takeWhile ((== xline) . fst) found
    lineys = map snd line
    ysRemaining = filter (`notElem` lineys) ys
    beamsSplit = filter (`elem` lineys) ys
    newBeams = concat $ makeNewYs beamsSplit

makeNewYs :: (Num a) => [a] -> [[a]]
makeNewYs ys = [[y - 1, y + 1] | y <- ys]

beamTravel [] _ count = count
beamTravel ls ys count = beamTravel newlist newys newcount
  where
    newlist = snd $ fst travelOnce
    newys = fst $ fst travelOnce
    newcount = count + snd travelOnce
    travelOnce = freeBeamTravel ls ys

makeTaggedYs :: (Num a) => a -> [(a, a)]
makeTaggedYs y = [(y, y - 1), (y, y + 1)]

timesplitTravel ::
  (Eq a2, Eq a1) =>
  [(a2, a1)] ->
  [a1] ->
  (([a1], [(a2, a1)]), [a1])
timesplitTravel ls ys = ((ysRemaining, rest), beamsSplit)
  where
    found = dropWhile ((`notElem` ys) . snd) ls
    xline = fst $ head found
    rest = dropWhile ((== xline) . fst) found
    line = takeWhile ((== xline) . fst) found
    lineys = map snd line
    ysRemaining = filter (`notElem` lineys) ys
    beamsSplit = filter (`elem` lineys) ys

beamTravel' :: (Eq a2, Num a, Num p, Ord a) => [(a2, a)] -> [a] -> M.Map a p -> p
beamTravel' [] _ dict = sum $ M.elems dict
beamTravel' ls ys dict = beamTravel' newlist newys newdict
  where
    newlist = snd $ fst travelOnce
    remys = fst $ fst travelOnce
    beamsSplit = snd travelOnce
    newys = nub $ remys ++ map snd newBeams
    newdict =
      foldl
        (\d (x, y) -> M.insertWith (+) y (M.findWithDefault 0 x dict) d)
        remainingDict
        newBeams
    travelOnce = timesplitTravel ls ys
    remainingDict = M.filterWithKey (keysInList (fst (fst travelOnce))) dict
    newBeams = concatMap makeTaggedYs beamsSplit

keysInList :: (Foldable t, Eq a) => t a -> a -> p -> Bool
keysInList ls k _ = k `elem` ls
