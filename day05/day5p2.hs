#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isInfixOf)
import Data.List.Split (chunksOf)

main :: IO ()
main = interact $ show . length . filter classify . lines
    where
        classify :: String -> Bool
        classify s = repeatingPairs s && oneSpaced s

        repeatingPairs s | length s < 2 = False
        repeatingPairs s = isInfixOf (take 2 s) (drop 2 s) || repeatingPairs (tail s)

        oneSpaced s = any id $ zipWith (==) s (tail $ tail s)
