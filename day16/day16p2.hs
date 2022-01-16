#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isPrefixOf)
import Data.List.Split (splitWhen)

main :: IO ()
main = interact $ fst . head . dropWhile ((/= 3) . length . filter id . map knownFacts . snd) . map parsein . lines

parsein :: String -> (String, [(String, Int)])
parsein str = let (sue,rst) = substringSplit ":" $ filter (/= ' ') str
                  kvs = splitWhen (==',') rst
              in (sue, (fmap read) <$> map (substringSplit ":") kvs)

knownFacts :: (String, Int) -> Bool
knownFacts ("children", i) = i == 3
knownFacts ("cats", i)     = i > 7
knownFacts ("samoyeds", i) = i == 2
knownFacts ("pomeranians", i) = i < 3
knownFacts ("akitas", i) = i == 0
knownFacts ("vizslas", i) = i == 0
knownFacts ("goldfish", i) = i < 5
knownFacts ("trees", i) = i > 3
knownFacts ("cars", i) = i == 2
knownFacts ("perfumes", i) = i == 1
knownFacts (s,_) = error ("Encountered property: " ++ s)

substringSplit :: String -> String -> (String, String)
substringSplit = substringSplit' ""

substringSplit' :: String -> String -> String -> (String, String)
substringSplit' buf pf [] = error "No match"
substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)
