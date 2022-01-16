#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isInfixOf)

main :: IO ()
main = interact $ head . dropWhile ((/= 3) . length . filter id . knownFacts) . lines

knownFacts :: String -> [Bool]
knownFacts ps = map (\f -> isInfixOf f ps) facts

facts :: [String]
facts = ["children: 3","cats: 7","samoyeds: 2","pomeranians: 3","akitas: 0","vizslas: 0","goldfish: 5","trees: 3","cars: 2","perfumes: 1"]
