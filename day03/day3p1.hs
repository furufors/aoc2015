#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (nub)
type XY = (Int, Int)

main :: IO ()
main = interact $ show . length . nub . foldl takeStep [(0, 0)] . head . lines
    where
        takeStep :: [XY] -> Char -> [XY]
        takeStep (s:ss) '^' = (fst s, snd s + 1):s:ss
        takeStep (s:ss) 'v' = (fst s, snd s - 1):s:ss
        takeStep (s:ss) '>' = (fst s + 1, snd s):s:ss
        takeStep (s:ss) '<' = (fst s - 1, snd s):s:ss
        takeStep ______ ___ = error "Illegal characther."
