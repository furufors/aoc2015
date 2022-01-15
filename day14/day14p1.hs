#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isPrefixOf)
type RaindeerStat = (Int, Int, Int) -- (Speed, Active, Resting)

main :: IO ()
main = interact $ show . maximum . map (\r -> sum . take 2503 $ generator r) . map parsein . lines

parsein :: String -> RaindeerStat
parsein str = let (_,rest) = substringSplit " can fly " str
                  (spd, rest1) = substringSplit " km/s for " rest
                  (acv, rest2) = substringSplit " seconds, but then must rest for " rest1
                  (rst, _) = span (`elem` ['0'..'9']) rest2
              in (read spd, read acv, read rst)
--Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.

generator :: RaindeerStat -> [Int]
generator (spd, active, resting) = cycle ((take active $ repeat spd) ++ (take resting $ repeat 0))

substringSplit :: String -> String -> (String, String)
substringSplit = substringSplit' ""

substringSplit' :: String -> String -> String -> (String, String)
substringSplit' buf pf [] = error "No match"
substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)
