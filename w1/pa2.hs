{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)

-- Core code

mpp :: [Int64] -> Int64
mpp xs = product $ take 2 $ sortBy (comparing Down) xs

-- IO plumbing

main :: IO ()
main = do
    s <- parse . lines <$> getContents
    print $ mpp s

parse :: [String] -> [Int64]
parse input = map (read::String->Int64) $ concat $ map words $ drop 1 input
