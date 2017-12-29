{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor  ((<$>))
import Data.List     ((\\), sortBy, unfoldr)

-- Core code

-- [23,39,92]

a :: [Int] -> [Int]
a xs = sortBy g xs where
    g x y
        | f x y == x = LT
        | f x y == y = GT
        | otherwise  = EQ

f :: Int -> Int -> Int
f x y = digitsToInt $ pick (intToDigits x) (intToDigits y)

pick :: [Int] -> [Int] -> [Int]
pick xs ys
    | length xs > length ys = pickByDigits xs ys
    | length xs < length ys = pickByDigits ys xs
    | otherwise             = if digitsToInt xs >= digitsToInt ys then xs else ys
    where
        digitDiff as bs = filter (/= 0) $ zipWith (-) as bs
        pickByDigits lng srt
            | digitDiff lng srt /= [] = if (head $ digitDiff lng srt) > 0 then lng else srt
            | otherwise               = if pick (lng \\ srt) srt == srt then srt else lng

intToDigits :: Int -> [Int]
intToDigits n = reverse $ unfoldr f (0, n) where
    f :: (Int, Int) -> Maybe (Int, (Int, Int))
    f (i, x)
        | x == 0    = if i == 0 then Just (0, (1, 0)) else Nothing
        | otherwise = Just (x `mod` 10, (i + 1, x `div` 10))

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\num d -> 10 * num + d) 0

-- IO plumbing

main :: IO ()
main = do
    xs <- parse <$> getContents
    display xs

parse :: String -> [Int]
parse = map (read::String->Int) . concat . map words . tail . lines

display :: [Int] -> IO ()
display = print
