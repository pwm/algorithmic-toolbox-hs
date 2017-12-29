{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor  ((<$>))
import Data.List     (sort, sortBy)
import Control.Monad (mapM_)

-- Core code

minPoints :: [(Int, Int)] -> (Int, [Int])
minPoints xys = foldr f (0, []) (s xys) where

    s :: [(Int, Int)] -> [(Int, Int)]
    s xs = sortBy g xs where
        g (_, y1) (_, y2)
            | y1 < y2   = GT
            | y1 > y2   = LT
            | otherwise = EQ

    f :: (Int, Int) -> (Int, [Int]) -> (Int, [Int])
    f (_, y) (_, []) = (1, [y])
    f (x, y) (n, ys@(yMax:_))
        | yMax < x  = (n + 1, y : ys)
        | otherwise = (n, ys)

-- IO plumbing

main :: IO ()
main = do
    xs <- parse <$> getContents
    display $ minPoints xs

parse :: String -> [(Int, Int)]
parse = map ((\[x, y] -> (x, y)) . map (read::String->Int) . words) . tail . lines

display :: (Int, [Int]) -> IO ()
display (n, ys) = do
    print n
    mapM_ (putStr . (\y -> y ++ " ") . show) $ sort ys
    putStr "\n"
