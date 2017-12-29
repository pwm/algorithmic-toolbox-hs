{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Functor ((<$>))

-- Core code

fib :: Integer -> Int -> Integer
fib m x = fibs !! x `mod` m where
    fibs = map fst $ iterate (\(a, b) -> (b, (a + b))) (0, 1)

--fibModSeq :: Integer -> [Int] -> [Integer]
--fibModSeq m xs = foldr (\x a -> (fib m x) : a) [] xs
--
--cyclic :: [Integer] -> Bool
--cyclic xs = length xs > 2 && drop (length xs `div` 2) xs == take (length xs `div` 2) xs
--
--cycleLength :: Integer -> Int
--cycleLength m = (length $ until (cyclic . fibModSeq m) (\xs -> xs ++ [(head $ reverse xs) + 1]) [0]) `div` 2

a001175 :: Int -> Int
a001175 1 = 1
a001175 m = f 1 ps 0 where
    f 0 (1 :  _) p = p
    f _ (x : xs) p = f x xs (p + 1)
    ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` m) (tail ps) ps

-- IO plumbing

main :: IO ()
main = do
    (x, m) <- parse . lines <$> getContents
    let res = fib (fromIntegral m) (x `mod` (a001175 (fromIntegral m)))
    print res

parse :: [String] -> (Int, Int)
parse = (\[x, m] -> (x, m)) . map (read::String->Int) . concat . map words
