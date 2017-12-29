{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))
import Data.List    (sortBy, unfoldr)

-- Core code

knapsack :: Double -> [(Double, Double)] -> [(Double, Double)]
knapsack wTotal vws = unfoldr f (wTotal, sortBy g vws) where
    f (_, []) = Nothing
    f (wT, ((v, w):remaining))
        | wT == 0   = Nothing
        | otherwise = Just (if wT > w then (v, w) else (wT / w * v, wT), (if wT > w then wT - w else 0, remaining))
    g (v1, w1) (v2, w2)
        | v1 / w1 > v2 / w2 = LT
        | v1 / w1 < v2 / w2 = GT
        | otherwise         = EQ

-- IO plumbing

main :: IO ()
main = do
    (_, wTotal):vws <- parse <$> getContents
    print $ sum $ map fst $ knapsack wTotal vws

parse :: String -> [(Double, Double)]
parse = map ((\[x, y] -> (x, y)) . (map (read::String->Double)) . words) . lines where
