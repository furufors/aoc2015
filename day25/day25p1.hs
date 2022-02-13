#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = putStrLn $ show $ codeNum !! ((toNum 3010 3019) - 1)

toNum :: Int -> Int -> Int
toNum row column = (sum [0..(row-1)] + 1) + sum (take (column-1) [(row+1), (row + 2)..])

codeNum :: [Integer]
codeNum = 20151125:(codeNum' 20151125)
    where
        codeNum' x = let next = (x * 252533) `mod` 33554393 in next:(codeNum' next)
