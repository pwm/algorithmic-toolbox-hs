{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))

ldfs :: Integer -> Int
ldfs x
    | x == 0    = 0
    | otherwise = xs !! (fromIntegral ((x - 1) `mod` 60))
    where xs = [1,2,4,7,2,0,3,4,8,3,2,6,9,6,6,3,0,4,5,0,6,7,4,2,7,0,8,9,8,8,7,6,4,1,6,8,5,4,0,5,6,2,9,2,2,5,8,4,3,8,2,1,4,6,1,8,0,9,0,0]

partialSum :: Integer -> Integer -> Int
partialSum x y
    | a >= b    = a - b
    | otherwise = 10 + a - b
    where
        a = ldfs y
        b = ldfs (x - 1)

-- IO plumbing

main :: IO ()
main = do
    (x, y) <- parse . lines <$> getContents
    print $ partialSum x y

parse :: [String] -> (Integer, Integer)
parse = (\[x, y] -> (x, y)) . map (read::String->Integer) . concat . map words
