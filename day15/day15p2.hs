#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List (isPrefixOf)
type IngredientAmount = [Int] -- capacity, durability, flavor, texture
type IngredientProperty = (Int, Int, Int, Int, Int) -- capacity, durability, flavor, texture

main :: IO ()
main = interact $ show . maximum . (\g -> toCost (permutations (length g) [[]]) g) . parMap rpar parsein . lines

parsein :: String -> IngredientProperty
-- Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
parsein str = let (  _, rest0) = substringSplit ": capacity " str
                  (cap, rest1) = substringSplit ", durability " rest0
                  (dur, rest2) = substringSplit ", flavor " rest1
                  (fla, rest3) = substringSplit ", texture " rest2
                  (tex, cal) = substringSplit ", calories " rest3
              in (read cap, read dur, read fla, read tex, read cal)

permutations :: Int -> [[Int]] -> [IngredientAmount]
permutations 0 is = []
permutations 1 is = map (\s -> (100-sum s:s)) is
permutations i is = permutations (i-1) [n:ns | ns <- is, n <- [0..(100-sum ns)]]

toCost :: [IngredientAmount] -> [IngredientProperty] -> [Int]
toCost ias ips = (map (calc ips) ias) `using` parListChunk 64 rdeepseq

calc :: [IngredientProperty] -> IngredientAmount -> Int
calc ps as = let score = \(a,b,c,d,e) -> if any (<0) [a,b,c,d] || e /= 500 then 0 else a*b*c*d
                 sumProp = foldl1 (\(a,b,c,d,e) (f,g,h,i,j) -> (a+f,b+g,c+h,d+i,e+j))
                 amoutProp = zipWith (\(a,b,c,d,e) n -> (n*a, n*b, n*c, n*d, n*e)) ps as
             in score . sumProp $ amoutProp

substringSplit :: String -> String -> (String, String)
substringSplit = substringSplit' ""

substringSplit' :: String -> String -> String -> (String, String)
substringSplit' buf pf [] = error "No match"
substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)
