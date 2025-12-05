module Day01 (day01a, day01ex, day01b) where

import Data.Bifunctor (bimap)
import Data.List (foldl', scanl')

data Direction = L | R deriving (Read, Show)

day01a :: IO ()
day01a = solveA "input/day01.txt"

day01b :: IO ()
day01b = solveB "input/day01.txt"

day01ex :: IO ()
day01ex = solveA "input/example01.txt"

makeRotations :: (Read b, Read d) => [Char] -> (b, d)
makeRotations s = bimap read read $ splitAt 1 s

rotateOnce :: (Integral a) => a -> (Direction, a) -> a
rotateOnce curr (L, n) = (curr - n) `mod` 100
rotateOnce curr (R, n) = (curr + n) `mod` 100

rotateAll :: [(Direction, Integer)] -> [Integer]
rotateAll = scanl' rotateOnce 50

countZeros :: [Integer] -> Int
countZeros = length . filter (== 0)

-- Part 2

zeroCrossed :: (Integral a) => a -> (Direction, a) -> a
zeroCrossed curr (R, n) = (curr + n) `div` 100
zeroCrossed 0 (_, n) = n `div` 100
zeroCrossed curr (_, n) = (100 - curr + n) `div` 100

rotateZeroCrossed :: (Integral b1) => (b1, b2) -> (Direction, b1) -> (b1, b1)
rotateZeroCrossed (x, _) y = (rotateOnce x y, zeroCrossed x y)

rotateZeroCrossedAll :: [(Direction, Integer)] -> [(Integer, Integer)]
rotateZeroCrossedAll = scanl' rotateZeroCrossed (50, 0)

countZeroCrossings :: [(a, Integer)] -> Integer
countZeroCrossings = sum . map (abs . snd)

solveA :: FilePath -> IO ()
solveA fname =
  readFile fname
    >>= ( print
            . countZeros
            . rotateAll
            . map makeRotations
            . lines
        )

solveB :: FilePath -> IO ()
solveB fname =
  readFile fname
    >>= ( print
            . countZeroCrossings
            . rotateZeroCrossedAll
            . map makeRotations
            . lines
        )
