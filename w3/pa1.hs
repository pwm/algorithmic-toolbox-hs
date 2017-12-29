{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))
import Data.List    (unfoldr)

-- Core code

change :: Int -> [Int]
change n = unfoldr f n where
    f n
        | n - 10 >= 0 = Just (10, n - 10)
        | n -  5 >= 0 = Just ( 5, n -  5)
        | n -  1 >= 0 = Just ( 1, n -  1)
        | otherwise   = Nothing

-- IO plumbing

main :: IO ()
main = do
    n <- parse <$> getContents
    print $ length $ change n []

parse :: String -> Int
parse = read::String->Int
