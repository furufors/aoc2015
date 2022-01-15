#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isPrefixOf, transpose)
type RaindeerStat = (Int, Int, Int) -- (Speed, Active, Resting)

main :: IO ()
main = interact $ show . maximum . score . map (\r -> cumsum 0 .take 2503 $ generator r) . map parsein . lines

score :: [[Int]] -> [Int]
score ss = map sum $ transpose . score' . transpose $ ss
    where
        score' :: [[Int]] -> [[Int]]
        score' [    ] = []
        score' (s:ss) = let lead = maximum s
                   in (map (\h -> if h == lead then 1 else 0) s):(score' ss)

cumsum :: Int -> [Int] -> [Int]
cumsum _ [    ] = []
cumsum i (a:as) = (i+a):(cumsum (i+a) as)

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
