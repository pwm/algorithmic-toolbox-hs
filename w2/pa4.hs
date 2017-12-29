{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))

-- Core code

gcd1 :: Integer -> Integer -> Integer
gcd1 x y
    | y == 0    = x
    | otherwise = gcd1 y (x `mod` y)

lcm1 :: Integer -> Integer -> Integer
lcm1 x y = (x * y) `div` (gcd1 x y)

-- IO plumbing

main :: IO ()
main = do
    (x, y) <- parse . lines <$> getContents
    print $ lcm1 x y

parse :: [String] -> (Integer, Integer)
parse = (\[x, y] -> if x > y then (x, y) else (y, x)) . map (read::String->Integer) . concat . map words
