module Day02(day02a, ex02a,ex02b, day02b)
where
import Data.List.Split(splitOn)
import Data.List(nub)
day02a = do
    input <- readFile "input/day02.txt"
    let ranges = sepRanges input
    let minmaxes = map findMinMax ranges 
    let duplicated = map (filter isDuplicated . uncurry allSequences) minmaxes
    print $ sum $ map read $ concat  duplicated
day02b = solve2 "input/day02.txt" isRepeatSequence
ex02a = solve2 "input/example02.txt" isDuplicated
ex02b = solve2 "input/example02.txt" isRepeatSequence
isDuplicated s =( take ((length s )`div ` 2) s) == (drop ((length s) `div` 2) s)

allNums :: Enum a => a -> a -> [a]
allNums minN maxN =  [minN..maxN]

allSequences :: (Show a, Enum a) => a -> a -> [String]
allSequences minN maxN = map show $ allNums minN maxN

findMinMax ::  [Char] -> (Int, Int)
findMinMax s = (head splitnums,last splitnums) 
    where splitnums = map read $ splitOn "-" s

sepRanges :: [Char] -> [[Char]]
sepRanges = splitOn ","

notDivisibleBy n x = x `mod` n /= 0
isRepeatSequence s = isMultiple [2..length s] s
isMultiple  ns s
    | null ns  = False    
    | isNRepeats s (head ns) = True

    | otherwise = isMultiple  (filter (notDivisibleBy (head ns)) ns) s

isNRepeats s n = go (length s `div` n) s 
    where go seqLen s 
            | ( take seqLen s) == (drop seqLen s) = True 
            | ( take seqLen s ) /= ( take seqLen (drop seqLen s)) = False
            | seqLen  > length s = False 
            | otherwise  = go seqLen (drop seqLen s)

multiples f = concatMap (nub . filter f . uncurry allSequences. findMinMax) 

solve2 fname f = readFile fname >>= print  . sum . map read .multiples f. sepRanges