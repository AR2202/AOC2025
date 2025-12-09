module Day08 (day08a, distanceToAll, allDistances, ex08a, ex08b, day08b) where

import Data.List (nub, sort, sortBy, sortOn, (\\))
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Set as S

type Point = (Float, Float, Float)

day08a :: IO ()
day08a = solve8a "input/Day08.txt" 1000

ex08a :: IO ()
ex08a = solve8a "input/example08.txt" 10

solve8a :: FilePath -> Int -> IO ()
solve8a fname n = do
  input <- readFile fname
  let ls = lines input
  let points = map toPoint ls

  let connectedn = connectN n points
  let pointsOnly = map fst connectedn

  let circs = (circuits . S.fromList) pointsOnly
  print $ product $ take 3 $ reverse $ sort circs

distance :: Floating a => (a, a, a) -> (a, a, a) -> a
distance (x, y, z) (a, b, c) = sqrt $ (x - a) ** 2 + (y - b) ** 2 + (z - c) ** 2

toPoint :: String -> Point
toPoint s = (read a, read b, read c)
  where
    a = head split
    b = head $ tail split
    c = last split
    split = splitOn "," s

distanceToAll :: [Point] -> Point -> [((Point, Point), Float)]
distanceToAll ls p = zip (zip (repeat p) ls) $ map (distance p) ls


allDistances :: [Point] -> [((Point, Point), Float)]
allDistances [] = []
allDistances (x : xs) = distanceToAll xs x ++ allDistances xs

connectN :: Int -> [Point] -> [((Point, Point), Float)]
connectN n points = take n $ sortBy (comparing snd) (allDistances points)



makeSubcircuit :: Ord a => S.Set (a, a) -> a -> S.Set a
makeSubcircuit xs x =
  if S.null xs then S.empty else S.union connected $ S.unions $ S.map (makeSubcircuit notconnected) connected
  where
    connected2fst = S.filter ((== x) . fst) xs
    connected2snd = S.filter ((== x) . snd) xs
    connected =
      S.union
        (S.map snd connected2fst)
        (S.map fst connected2snd)

    notconnected = xs S.\\ S.union connected2fst connected2snd


circuits :: Ord b => S.Set (b, b) -> [Int]
circuits xs = if S.null xs then [] else S.size inCircuitX : circuits newXs
  where
    inCircuitX = S.insert (fst (S.elemAt 0 xs)) $ makeSubcircuit xs (fst (S.elemAt 0 xs))

    fstInX = S.filter ((`S.member` inCircuitX) . fst) xs
    sndInX = S.filter ((`S.member` inCircuitX) . snd) xs
    newXs = (xs S.\\ sndInX) S.\\ fstInX

-- part2
ex08b :: IO ()
ex08b = solve8b "input/example08.txt"

day08b :: IO ()
day08b = solve8b "input/Day08.txt"

solve8b :: FilePath -> IO ()
solve8b fname = do
  input <- readFile fname
  let ls = lines input
  let points = map toPoint ls
  let connections = map fst $ connectAll points
  -- print connections
  let lastMerge = mergeUntilAllConnected (S.fromList points) connections M.empty

  print $ uncurry multiplyX lastMerge

connectAll :: [Point] -> [((Point, Point), Float)]
connectAll points = sortBy (comparing snd) (allDistances points)

mergeUntilAllConnected _ [] _ = ((0, 0, 0), (0, 0, 0))
mergeUntilAllConnected allpoints (conn : conns) dict =
  case M.lookup (fst conn) dict of
    Just t -> case M.lookup (snd conn) dict of
      Just u ->
        if S.union u t == allpoints
          then conn
          else
            mergeUntilAllConnected allpoints conns $
              M.mapWithKey (\k v -> if k `S.member` S.union u t then S.union u t else v) dict
      Nothing ->
        if S.insert (snd conn) t == allpoints
          then conn
          else
            mergeUntilAllConnected allpoints conns $
              M.mapWithKey (\k v -> if k `S.member` t then S.insert (snd conn) t else v) $
                M.insert (snd conn) (S.insert (snd conn) t) dict
    Nothing -> case M.lookup (snd conn) dict of
      Nothing ->
        mergeUntilAllConnected allpoints conns $
          M.insert (fst conn) (S.insert (fst conn) (S.singleton (snd conn))) $
            M.insert (snd conn) (S.insert (snd conn) (S.singleton (fst conn))) dict
      Just s ->
        if S.insert (fst conn) s == allpoints
          then conn
          else
            mergeUntilAllConnected allpoints conns $
              M.mapWithKey (\k v -> if k `S.member` s then S.insert (fst conn) s else v) $
                M.insert (fst conn) (S.insert (fst conn) s) dict

multiplyX (x, _, _) (a, _, _) = round x * round a
