#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = interact $ show . sum . toListOfInt . head . lines
    where
        toListOfInt :: String -> [Int]
        toListOfInt [] = []
        toListOfInt str = let (a,b) = span (\c -> elem c ('-':['0'..'9'])) str
                          in if a == [] then toListOfInt (tail str) else (read a):(toListOfInt b)
