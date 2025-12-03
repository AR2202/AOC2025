module Day03(day03) where
import Data.List.Split (chunksOf)

day03 :: IO ()
day03 = do 
    input <- readFile  "input/Day03.txt"
    let banks = lines input 
    let joltage = map (maximumJoltage . makeIntoDigits) banks
    let joltages = map (find12Batteries . makeIntoDigits) banks
    putStrLn "Part 1:"
    print $ sum joltage
    putStrLn "Part 2:"
    print $ sum joltages


maximumBattery :: [Int] -> Int
maximumBattery = maximum . init 

makeIntoDigits :: String -> [Int]
makeIntoDigits = map (read . return)

maxAfterMax maxBattery  = maximum . tail . dropWhile  (/= maxBattery)

maximumJoltage :: [Int] -> Int
maximumJoltage bank = 10 * maxBattery + maxAfterMax maxBattery bank
    where maxBattery = maximumBattery bank
-- Part 2
nthBattery :: Ord a => [a] -> Int -> a
nthBattery  bank n = maximum $ take (length bank - 12 + n ) bank

find12Batteries bank = go bank 1 
    where go _ 13  = 0
          go bank' n = 10^(12-n) * maxBattery + go ((tail . dropWhile  (/= maxBattery)) bank') (n+1) 
           where maxBattery = nthBattery bank' n 