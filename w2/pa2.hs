{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))

-- Core code

fib :: Int -> Int
fib x = fibs !! x where
    fibs = map fst $ iterate (\(a, b) -> (b `mod` 10, (a + b) `mod` 10)) (0, 1)

-- IO plumbing

main :: IO ()
main = do
    x <- parse . lines <$> getContents
    print $ fib x

parse :: [String] -> Int
parse = head . map (read::String->Int)
