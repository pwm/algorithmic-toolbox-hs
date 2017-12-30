{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor  ((<$>))
import Data.List     ((\\), sortBy, unfoldr)

-- Core code

largest :: [Int] -> Int
largest xs = intListToInt $ sortBy bestPick xs where
    bestPick x y
        | pickFrom x y == x = LT
        | pickFrom x y == y = GT
        | otherwise         = EQ
    pickFrom x y = digitsToInt $ cmpByDigits (intToDigits x) (intToDigits y)

cmpByDigits :: [Int] -> [Int] -> [Int]
cmpByDigits xs ys
    | length xs > length ys = pickByDigits xs ys
    | length xs < length ys = pickByDigits ys xs
    | otherwise             = if digitsToInt xs >= digitsToInt ys then xs else ys
    where
        digitDiff as bs = filter (/= 0) $ zipWith (-) as bs
        pickByDigits lng srt
            | digitDiff lng srt /= [] = if (head $ digitDiff lng srt) > 0 then lng else srt
            | otherwise               = if cmpByDigits (lng \\ srt) srt == srt then srt else lng

intToDigits :: Int -> [Int]
intToDigits n = reverse $ unfoldr f (0, n) where
    f :: (Int, Int) -> Maybe (Int, (Int, Int))
    f (i, x)
        | x == 0    = if i == 0 then Just (0, (1, 0)) else Nothing
        | otherwise = Just (x `mod` 10, (i + 1, x `div` 10))

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\num d -> 10 * num + d) 0

intListToInt :: [Int] -> Int
intListToInt = digitsToInt . concat . map intToDigits

-- IO plumbing

main :: IO ()
main = do
    xs <- parse <$> getContents
    display $ largest xs

parse :: String -> [Int]
parse = map (read::String->Int) . concat . map words . tail . lines

display :: Int -> IO ()
display = print
