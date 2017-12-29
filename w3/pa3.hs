{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))
import Data.List    (sortBy)
import Data.Ord     (Down(..), comparing)

-- Core code

maxRev :: [Int] -> [Int] -> Int
maxRev xs ys = sum $ map (\(x, y) -> x * y) $ zip (s xs) (s ys) where
    s = sortBy (comparing Down)

-- IO plumbing

main :: IO ()
main = do
    (xs, ys) <- parse <$> getContents
    print $ maxRev xs ys

parse :: String -> ([Int], [Int])
parse = (\[x, y] -> (x, y)) . map (map (read::String->Int) . words) . tail . lines
