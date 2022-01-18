#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = putStrLn $ show . (+1) . length . takeWhile ((<36000000) . toCount) $ [1..]

toCount :: Int -> Int
toCount n = sum [10 * x | x <- [1..n], n `mod` x == 0]
