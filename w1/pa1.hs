{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))
import Data.Int (Int64)

-- Core code

add :: [Int64] -> Int64
add = foldr (+) 0

-- IO plumbing

main :: IO ()
main = do
    xs <- parse . lines <$> getContents
    print $ add xs

parse :: [String] -> [Int64]
parse input = map (read::String->Int64) $ concat $ map words $ input
