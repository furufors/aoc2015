#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isInfixOf)

main :: IO ()
main = interact $ show . length . filter classify . lines
    where
        classify :: String -> Bool
        classify s = hasThreeVowels s && hasRepeating s && noNaughtyStrings s

        hasThreeVowels = (>= 3) . length . filter (`elem` "aeiou")

        hasRepeating s = any id $ zipWith (==) s (tail s)

        noNaughtyStrings s = all (\e -> not $ isInfixOf e s) ["ab", "cd", "pq", "xy"]
