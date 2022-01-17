#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ show . length . nub . sort . create . parsein . lines

create :: ([(String, String)], String) -> [String]
create ([    ], _____) = []
create ((r:rs), input) = apply r input ++ create (rs, input)

apply :: (String, String) -> String -> [String]
apply (f,t) str = [ res
                  | let others = splitOn f str
                  , let n = length others
                  , n > 1
                  , i <- [1..(n-1)]
                  , let res = (intercalate f $ take i others) ++ t ++ (intercalate f $ drop i others)
                  ]

parsein :: [String] -> ([(String, String)], String)
parsein str = let (calib, input) = head . tail <$> span (/= []) str
              in (map (substringSplit " => ") calib, input)

substringSplit :: String -> String -> (String, String)
substringSplit = substringSplit' ""

substringSplit' :: String -> String -> String -> (String, String)
substringSplit' buf pf [] = error "No match"
substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)
