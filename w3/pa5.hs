{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor  ((<$>))
import Control.Monad (mapM_)
import Data.List     (unfoldr)

-- Core code

ms :: Int -> [Int]
ms n = unfoldr f (0, n) where
    f (i, x)
        | x == 0    = Nothing
        | otherwise = Just(g (i + 1) x, ((i + 1), x - g (i + 1) x))
    g i x
        | x `mod` 2 == 0 = if i < x `div` 2     then i else x
        | otherwise      = if i < x `div` 2 + 1 then i else x

-- IO plumbing

main :: IO ()
main = do
    n <- parse <$> getContents
    display $ ms n

parse :: String -> Int
parse = read::String->Int

display :: [Int] -> IO ()
display ns = do
    print $ length ns
    mapM_ (putStr . (\n -> n ++ " ") . show)  ns
    putStr "\n"
