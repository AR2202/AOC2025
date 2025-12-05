module Day05(day05) where 
import Data.List.Split(splitOn)
import Data.List(sort)
import Data.Bits (Bits(xor))

day05 :: IO ()
day05 = do
    input <- readFile "input/Day05.txt"
    let splitOnBlank = splitOn "\n\n" input
    let ranges = map (list2tuple . map read . splitOn "-") $ lines $ head splitOnBlank
    let ingredients = map read $ lines $ last splitOnBlank 
   

    print $ length $ filter (inAnyRange ranges) ingredients
    print $ countInRanges $ mergeRanges ranges

mergeRanges :: Ord a => [(a, a)] -> [(a, a)]
mergeRanges [] = []
mergeRanges ls = go (head sorted) (tail sorted)
    where sorted = sort ls 
          go c [] = [c]
          go (a,b) ((x,y):xs) 
            | x > b = (a,b) : go (x,y) xs 
            |y <= b = go (a, b) xs 
            | otherwise = go (a,y) xs
list2tuple :: [b] -> (b, b)
list2tuple ls = (head ls, last ls)

inRange :: Ord a => a -> (a, a) -> Bool
inRange c (x,y) = c >= x && c<= y


inAnyRange :: [(Int,Int)] -> Int -> Bool
inAnyRange  xs c = any   (inRange c) xs

countInRange (x,y) = 1 + y-x 

countInRanges = sum . map countInRange