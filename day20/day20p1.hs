#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (nub)
main :: IO ()
main = putStrLn $ show . fst . head . dropWhile (not . snd) $ map toCount [2..]

threshold :: Int
threshold = 36000000 `div` 10 -- Offset problem by a factor 10

toCount :: Int -> (Int, Bool)
toCount a = let sqroot = ceiling . sqrt . fromIntegral $ a
            in (a, (>threshold) . sum . nub . concat $ [ [q, d] | d <- [1..sqroot], let (q,r) = a `divMod` d, r == 0])
