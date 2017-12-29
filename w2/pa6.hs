{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))
--import Control.Monad (foldM)

-- Core code

--fibSeq :: Int -> [Integer]
--fibSeq x = take (x + 1) fibs
--    where
--        --fibs = map fst $ iterate (\(a, b) -> (b `mod` 10, (a + b) `mod` 10)) (0, 1)
--        fibs = map fst $ iterate (\(a, b) -> (b, (a + b))) (0, 1)
--
--f :: () -> Int -> IO ()
--f acc x = do
--    putStr . show $ (sum $ fibSeq x) `mod` 10
--    putStr ","
--
--g :: Int -> IO ()
--g x = foldM f () [1..x]

lastDigitFibSum :: Integer -> Int
lastDigitFibSum x
    | x == 0    = 0
    | otherwise = xs !! (fromIntegral ((x - 1) `mod` 60))
    where xs = [1,2,4,7,2,0,3,4,8,3,2,6,9,6,6,3,0,4,5,0,6,7,4,2,7,0,8,9,8,8,7,6,4,1,6,8,5,4,0,5,6,2,9,2,2,5,8,4,3,8,2,1,4,6,1,8,0,9,0,0]

-- IO plumbing

main :: IO ()
main = do
    x <- parse . lines <$> getContents
    print $ lastDigitFibSum x

parse :: [String] -> Integer
parse = head . map (read::String->Integer)
